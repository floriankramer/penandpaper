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
      _action(Action::NONE),
      _tile_size(128),
      _brush(Brush::ADD),
      _brush_size(20) {
  setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
  // Start worker threads
  for (size_t i = 0; i < _num_workers; ++i) {
    std::thread t(&MapWidget::loadTiles, this, _workers_should_run);
    t.detach();
  }
  setMouseTracking(true);
}

MapWidget::~MapWidget() {
  // ensure all workers terminate
  *_workers_should_run = false;
  _tiles_to_load.signal(_num_workers);
}

void MapWidget::setMap(std::shared_ptr<Map> map) {
  _map = map;
  _camera.setPosition(_map->widthPixels() / 2, _map->heightPixels() / 2);
  _camera.setHeight(_map->heightPixels());
  _camera.setAspectRatio(double(width()) / height());
  _tile_cache.clear();
  repaint();
}

void MapWidget::exportMap(const std::string &path) {
  std::string save_to = path;
  if (save_to.size() < 4 || save_to.substr(save_to.size() - 4) != ".png") {
    save_to += ".png";
  }
  Image img = _tile_renderer.renderTile(_map.get(), 0, 0, _map->widthPixels(),
                                        _map->heightPixels());
  img.save(save_to);
}

void MapWidget::setBrush(Brush brush) { _brush = brush; }

void MapWidget::setBrushSize(size_t size) { _brush_size = size; }

size_t MapWidget::brushSize() const { return _brush_size; }

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
  if (event->button() == Qt::MouseButton::MiddleButton) {
    _action = Action::PAN;
  } else if (event->button() == Qt::MouseButton::LeftButton) {
    _action = Action::DRAW;
    int64_t world_x = screenToWorldX(event->x());
    int64_t world_y = screenToWorldY(event->y());
    applyBrush(world_x, world_y, world_x, world_y);
  }

  _last_mouse_x = event->x();
  _last_mouse_y = event->y();
}

void MapWidget::mouseMoveEvent(QMouseEvent *event) {
  if (_action == Action::PAN) {
    int delta_x = event->x() - _last_mouse_x;
    int delta_y = event->y() - _last_mouse_y;
    double screen_to_world = _camera.height() / height();
    _camera.move(-delta_x * screen_to_world, -delta_y * screen_to_world);
  } else if (_action == Action::DRAW) {
    int64_t start_x = screenToWorldX(_last_mouse_x);
    int64_t start_y = screenToWorldY(_last_mouse_y);
    int64_t stop_x = screenToWorldX(event->x());
    int64_t stop_y = screenToWorldY(event->y());
    applyBrush(start_x, start_y, stop_x, stop_y);
  }
  _last_mouse_x = event->x();
  _last_mouse_y = event->y();
  repaint();
}

void MapWidget::mouseReleaseEvent(QMouseEvent *event) {
  if (_action == Action::DRAW) {
    int64_t start_x = screenToWorldX(_last_mouse_x);
    int64_t start_y = screenToWorldY(_last_mouse_y);
    int64_t stop_x = screenToWorldX(event->x());
    int64_t stop_y = screenToWorldY(event->y());
    applyBrush(start_x, start_y, stop_x, stop_y);
  }
  _action = Action::NONE;
}

void MapWidget::paintEvent(QPaintEvent *event) {
  QPainter painter(this);
  painter.fillRect(0, 0, width(), height(), QColor(0, 0, 0));
  if (_map == nullptr) {
    return;
  }

  // compute the bounds of the visible tiles
  double cam_upper_left_x = _camera.x() - _camera.width() / 2;
  int64_t min_tile_x = std::floor(cam_upper_left_x / _tile_size) - 1;
  min_tile_x = std::max(int64_t(0), min_tile_x);

  double cam_upper_left_y = _camera.y() - _camera.height() / 2;
  int64_t min_tile_y = std::floor(cam_upper_left_y / _tile_size) - 1;
  min_tile_y = std::max(int64_t(0), min_tile_y);

  int64_t width_tiles = std::ceil(_camera.width() / _tile_size);
  int64_t max_tile_x = std::min(int64_t(_map->widthPixels() / _tile_size),
                                int64_t(min_tile_x + width_tiles + 3));

  int64_t height_tiles = std::ceil(_camera.height() / _tile_size);
  int64_t max_tile_y = std::min(int64_t(_map->heightPixels() / _tile_size),
                                int64_t(min_tile_y + height_tiles + 3));

  // compute how large a tile of the given size will be
  double zoom = height() / _camera.height();
  int tile_size_pixels = _tile_size * zoom;

  // compute the screen locations of all tiles as offsets to these coordinates
  // to avoid gaps appearing in between tiles
  int screen_base_x = worldToScreenX(min_tile_x * _tile_size);
  int screen_base_y = worldToScreenY(min_tile_y * _tile_size);

  for (size_t row = min_tile_y; row < size_t(max_tile_y); ++row) {
    int screen_y = screen_base_y + (row - min_tile_y) * tile_size_pixels;

    for (size_t col = min_tile_x; col < size_t(max_tile_x); ++col) {
      int screen_x = screen_base_x + (col - min_tile_x) * tile_size_pixels;
      QRect target(screen_x, screen_y, tile_size_pixels, tile_size_pixels);
      QRect src(0, 0, _tile_size, _tile_size);

      std::shared_ptr<QImage> img = renderTile(col, row);
      painter.drawImage(target, *img, src);
    }
  }

  // Draw a brush size preview
  painter.setPen(QColor(0, 255, 255));
  size_t visible_brush_size = _brush_size * zoom;
  painter.drawEllipse(_last_mouse_x - visible_brush_size,
                      _last_mouse_y - visible_brush_size,
                      2 * visible_brush_size, 2 * visible_brush_size);
}

int64_t MapWidget::worldToScreenX(int64_t world_x) const {
  double upper_left = _camera.x() - _camera.width() / 2;
  double pixel_per_meter = height() / _camera.height();
  return (world_x - upper_left) * pixel_per_meter;
}

int64_t MapWidget::worldToScreenY(int64_t world_y) const {
  double upper_left = _camera.y() - _camera.height() / 2;
  double pixel_per_meter = height() / _camera.height();
  return (world_y - upper_left) * pixel_per_meter;
}

int64_t MapWidget::screenToWorldX(int64_t screen_x) const {
  double upper_left = _camera.x() - _camera.width() / 2;
  double zoom = height() / _camera.height();
  return screen_x / zoom + upper_left;
}

int64_t MapWidget::screenToWorldY(int64_t screen_y) const {
  double upper_left = _camera.y() - _camera.height() / 2;
  double zoom = height() / _camera.height();
  return screen_y / zoom + upper_left;
}

std::shared_ptr<QImage> MapWidget::renderTile(size_t col, size_t row) {
  size_t idx = col + row * (_map->widthPixels() / _tile_size);

  auto tile_it = _tile_cache.find(idx);
  if (tile_it == _tile_cache.end()) {
    // render the image
    QImage img = Conversions::qImageFromImage(_tile_renderer.renderTile(
        _map.get(), col * _tile_size, row * _tile_size, (col + 1) * _tile_size,
        (row + 1) * _tile_size));
    // move the image to the heap and initialize the reference counting
    std::shared_ptr<QImage> ptr = std::make_shared<QImage>(std::move(img));
    // store the shared ptr in the cache
    _tile_cache[idx] = ptr;
    return ptr;
  } else {
    // return the cached tile
    return tile_it->second;
  }
}

void MapWidget::applyBrush(int64_t start_x, int64_t start_y, int64_t stop_x,
                           int64_t stop_y) {
  Map::BoundingBox changed;
  switch (_brush) {
    case Brush::SUBTRACT:
      changed = _map->softBrushStroke(start_x, start_y, stop_x, stop_y,
                                      _brush_size, false);
      break;
    case Brush::NOISE:
      changed =
          _map->noiseBrushStroke(start_x, start_y, stop_x, stop_y, _brush_size);
      break;
    case Brush::SMOOTHE:
      changed = _map->smoothBrushStroke(start_x, start_y, stop_x, stop_y,
                                        _brush_size);
      break;
    default:
      changed = _map->softBrushStroke(start_x, start_y, stop_x, stop_y,
                                      _brush_size, true);
      break;
  }
  invalidateCells(changed);
}

void MapWidget::invalidateCells(const Map::BoundingBox &aabb) {
  Map::BoundingBox cell_aabb;
  cell_aabb.min_x = aabb.min_x / _tile_size;
  cell_aabb.min_y = aabb.min_y / _tile_size;
  cell_aabb.max_x = std::ceil(aabb.max_x / _tile_size + 1);
  cell_aabb.max_y = std::ceil(aabb.max_y / _tile_size + 1);

  for (int64_t row = cell_aabb.min_y; row < cell_aabb.max_y; ++row) {
    for (int64_t col = cell_aabb.min_x; col < cell_aabb.max_x; ++col) {
      size_t idx = col + row * (_map->widthPixels() / _tile_size);
      auto it = _tile_cache.find(idx);
      if (it != _tile_cache.end()) {
        _tile_cache.erase(it);
      }
    }
  }
  repaint();
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
