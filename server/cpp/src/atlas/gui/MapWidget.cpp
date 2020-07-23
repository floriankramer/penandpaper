#include "MapWidget.h"

#include <QMouseEvent>
#include <QPainter>
#include <QSizePolicy>
#include <QWheelEvent>
#include <cmath>
#include <thread>

#include "../Logger.h"
#include "Conversions.h"

namespace atlas {
MapWidget::MapWidget(QWidget *parent)
    : QWidget(parent),
      _num_workers(2),
      _workers_should_run(new bool(true)),
      _is_moving(true),
      _tile_size(512) {
  setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
  // Start worker threads
  for (size_t i = 0; i < _num_workers; ++i) {
    std::thread t(&MapWidget::loadTiles, this, _workers_should_run);
    t.detach();
  }
}

MapWidget::~MapWidget() {
  // ensure all workers terminate
  *_workers_should_run = false;
  _tiles_to_load.signal(_num_workers);
}

void MapWidget::setMap(std::shared_ptr<Map> map) {
  _map = map;
  _camera.setPosition(0, 0);
  _camera.setHeight(_map->height(0));
  _camera.setAspectRatio(double(width()) / height());
  repaint();
}


void MapWidget::resizeEvent(QResizeEvent *event) {
  _camera.setAspectRatio(double(width()) / height());
  repaint();
}

void MapWidget::wheelEvent(QWheelEvent *event) {
  if (event->angleDelta().y() > 0) {
    _camera.zoom(0.9);
  } else {
    _camera.zoom(1 / 0.9);
  }
  repaint();
}

void MapWidget::mousePressEvent(QMouseEvent *event) {
  if (event->button() == Qt::MouseButton::LeftButton) {
    _is_moving = true;
  }
  _last_mouse_x = event->x();
  _last_mouse_y = event->y();
}

void MapWidget::mouseMoveEvent(QMouseEvent *event) {
  if (_is_moving) {
    int delta_x = event->x() - _last_mouse_x;
    int delta_y = event->y() - _last_mouse_y;
    double screen_to_world = _camera.height() / height();
    _camera.move(-delta_x * screen_to_world, -delta_y * screen_to_world);
    repaint();
  }
  _last_mouse_x = event->x();
  _last_mouse_y = event->y();
}

void MapWidget::mouseReleaseEvent(QMouseEvent *event) {
  if (event->button() == Qt::MouseButton::LeftButton) {
    _is_moving = false;
  }
}

void MapWidget::paintEvent(QPaintEvent *event) {
  QPainter painter(this);
  painter.fillRect(0, 0, width(), height(), QColor(0, 0, 0));
  if (_map == nullptr) {
    return;
  }

  size_t zoom_level = currentZoomLevel();

  // compute the bounds of the visible tiles
  double cam_upper_left_x = _camera.x() - _camera.width() / 2;
  int64_t min_tile_x = worldXCoordToTile(cam_upper_left_x, zoom_level) - 1;
  min_tile_x = std::max(int64_t(0), min_tile_x);

  double cam_upper_left_y = _camera.y() - _camera.height() / 2;
  int64_t min_tile_y = worldYCoordToTile(cam_upper_left_y, zoom_level) - 1;
  min_tile_y = std::max(int64_t(0), min_tile_y);

  int64_t width_tiles = std::ceil(_camera.width() / _map->width(zoom_level));
  int64_t max_tile_x =
      std::min(int64_t(_map->colsAtZoom(zoom_level)), min_tile_x + width_tiles + 3);

  int64_t height_tiles = std::ceil(_camera.height() / _map->height(zoom_level));
  int64_t max_tile_y =
      std::min(int64_t(_map->rowsAtZoom(zoom_level)), min_tile_y + height_tiles + 3);

  // compute how large a tile of the given size will be
  double pixel_per_meter = height() / _camera.height();
  int tile_size_pixels = _map->width(zoom_level) * pixel_per_meter;

  // compute the screen locations of all tiles as offsets to these coordinates
  // to avoid gaps appearing in between tiles
  int screen_base_x = worldToScreenX(tileXCoordToWorld(min_tile_x, zoom_level));
  int screen_base_y = worldToScreenY(tileYCoordToWorld(min_tile_y, zoom_level));

  for (size_t row = min_tile_y; row < size_t(max_tile_y); ++row) {
    int screen_y = screen_base_y + (row - min_tile_y) * tile_size_pixels;

    for (size_t col = min_tile_x; col < size_t(max_tile_x); ++col) {
      int screen_x = screen_base_x + (col - min_tile_x) * tile_size_pixels;
      QRect target(screen_x, screen_y, tile_size_pixels, tile_size_pixels);
      QRect src(0, 0, _tile_size, _tile_size);

      std::shared_ptr<QImage> img = renderTile(col, row, zoom_level);
      painter.drawImage(target, *img, src);
    }
  }
}


int MapWidget::worldToScreenX(double world_x) const {
  double upper_left = _camera.x() - _camera.width() / 2;
  double pixel_per_meter = height() / _camera.height();
  return (world_x - upper_left) * pixel_per_meter;
}

int MapWidget::worldToScreenY(double world_y) const {
  double upper_left = _camera.y() - _camera.height() / 2;
  double pixel_per_meter = height() / _camera.height();
  return (world_y - upper_left) * pixel_per_meter;
}

std::shared_ptr<QImage> MapWidget::renderTile(size_t col, size_t row,
                                              size_t zoom) {
  size_t idx = col + row * _map->cols() + zoom * _map->cols() * _map->rows();
  auto tile_it = _tile_cache.find(idx);
  if (tile_it == _tile_cache.end()) {
    // render the image
    QImage img = Conversions::qImageFromImage(_tile_renderer.renderTile(
        _map.get(), col, row, zoom, _tile_size, _tile_size));
    // move the image to the heap and initialize the reference counting
    std::shared_ptr<QImage> ptr =
        std::make_shared<QImage>(std::move(img));
    // store the shared ptr in the cache
    _tile_cache[idx] = ptr;
    return ptr;
  } else {
    // return the cached tile
    return tile_it->second;
  }
}

double MapWidget::tileXCoordToWorld(size_t coord, size_t zoom) const {
  // Do two transformations:
  // 1. Transform from a column index to an offset from the top left of the map
  // 2. center the map in the world coordinate system by subtracting half its
  //    width
  double x = coord * _map->width(zoom);
  return x - _map->width() / 2;
}

double MapWidget::tileYCoordToWorld(size_t coord, size_t zoom) const {
  // Do two transformations:
  // 1. Transform from a row index to an offset from the top left of the map
  // 2. center the map in the world coordinate system by subtracting half its
  //    height
  double y = coord * _map->height(zoom);
  return y - _map->height() / 2;
}

int64_t MapWidget::worldXCoordToTile(double world_x, size_t zoom) const {
  // Do two transformations:
  // 1. Add half the map width, as the map is centered
  // 2. Divide by the cell size
  double x = world_x + _map->width() / 2;
  return std::floor(x / _map->width(zoom));
}

int64_t MapWidget::worldYCoordToTile(double world_y, size_t zoom) const {
  // Do two transformations:
  // 1. Add half the map width, as the map is centered
  // 2. Divide by the cell size
  double y = world_y + _map->height() / 2;
  return std::floor(y / _map->height(zoom));
}

size_t MapWidget::currentZoomLevel() const {
  double visible_height = _camera.height();
  double map_height = _map->height();
  double factor = map_height / visible_height;

  // don't go lower than the base zoom level
  factor = std::max(1.0, factor);

  // Every increase in the zoom level subdivides the entire map, doubling the
  // number of tiles.
  size_t level = std::ceil(std::log2(factor));

  // Limit the zoom level
  return std::min(level, size_t(6));
}

void MapWidget::loadTiles(std::shared_ptr<bool> should_run) {
  while (*should_run) {
    _tiles_to_load.wait();
    if (!*should_run) {
      break;
    }
  }
}
}  // namespace atlas
