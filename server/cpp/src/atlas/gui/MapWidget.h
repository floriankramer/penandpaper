#pragma once

#include <QWidget>
#include <memory>
#include <unordered_map>

#include "Camera.h"

#include "core/Semaphore.h"
#include "core/Map.h"
#include "rendering/TileRenderer.h"

namespace atlas {
class MapWidget : public QWidget {
  Q_OBJECT

  struct TileCoordinate {
    size_t x, y, zoom;
  };

 public:
  MapWidget(QWidget *parent = nullptr);
  virtual ~MapWidget();

  void setMap(std::shared_ptr<Map> map);

 protected:
  void paintEvent(QPaintEvent *event) override;
  void wheelEvent(QWheelEvent *event) override;
  void mousePressEvent(QMouseEvent *event) override;
  void mouseMoveEvent(QMouseEvent *event) override;
  void mouseReleaseEvent(QMouseEvent *event) override;

  void resizeEvent(QResizeEvent *event) override;

 private:
  /**
   * @brief To be run on another thread to continuosly load tiles
   */
  void loadTiles(std::shared_ptr<bool> should_run);

  /**
   * @brief Returns an image of the rendererd tile. May return a cached result.
   */
  std::shared_ptr<QImage> renderTile(size_t col, size_t row, size_t zoom);

  /**
   * @brief Transforms a map tiles column to a position in world space
   */
  double tileXCoordToWorld(size_t coord, size_t zoom) const;

  /**
   * @brief Transforms a map tiles row to a position in world space
   */
  double tileYCoordToWorld(size_t coord, size_t zoom) const;

  /** @brief Transforms a world coordinate to a tile col. May be negative. */
  int64_t worldXCoordToTile(double world_x, size_t zoom) const;

  /** @brief Transforms a world coordinate to a tile row. May be negative. */
  int64_t worldYCoordToTile(double world_y, size_t zoom) const;

  /** @brief Converts from the world coordinate system to screen coordinates. */
  int worldToScreenX(double world_x) const;

  /** @brief Converts from the world coordinate system to screen coordinates. */
  int worldToScreenY(double world_y) const;

  /**
   * @brief Computes the current zoom level that should be used when rendering
   *        tiles.
   */
  size_t currentZoomLevel() const;

  std::shared_ptr<Map> _map;

  Camera _camera;

  TileRenderer _tile_renderer;

  size_t _num_workers;

  std::shared_ptr<bool> _workers_should_run;
  Semaphore _tiles_to_load;

  bool _is_moving;
  int _last_mouse_x, _last_mouse_y;

  int _tile_size;

  std::unordered_map<size_t, std::shared_ptr<QImage>> _tile_cache;
};

}  // namespace atlas
