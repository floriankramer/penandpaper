#pragma once

#include <QWidget>
#include <memory>
#include <unordered_map>

#include "Camera.h"
#include "core/Map.h"
#include "core/Semaphore.h"
#include "rendering/TileRenderer.h"

namespace atlas {
/**
 * @brief This widget renders a map. The map is first rendered into _tile_size
 * sized square images which are then rendered onto the screen.
 */
class MapWidget : public QWidget {
  Q_OBJECT

  struct TileCoordinate {
    size_t x, y, zoom;
  };

  enum class Action { NONE, PAN, DRAW };

 public:
  enum class Brush { ADD, SUBTRACT, NOISE, SMOOTHE, SHARPEN };

  MapWidget(QWidget *parent = nullptr);
  virtual ~MapWidget();

  void setMap(std::shared_ptr<Map> map);

  void exportMap(const std::string &path);
  void setBrush(Brush brush);

  void setBrushSize(size_t size);
  size_t brushSize() const;

  void setBrushStrength(float strength);
  float brushStrength() const;

  void undo();

 protected:
  void paintEvent(QPaintEvent *event) override;
  void wheelEvent(QWheelEvent *event) override;
  void mousePressEvent(QMouseEvent *event) override;
  void mouseMoveEvent(QMouseEvent *event) override;
  void mouseReleaseEvent(QMouseEvent *event) override;

  void resizeEvent(QResizeEvent *event) override;

 private:
  void applyBrush(int64_t start_x, int64_t start_y, int64_t stop_x,
                  int64_t stop_y);

  void invalidateCells(const Map::BoundingBox &aabb);

  /**
   * @brief To be run on another thread to continuosly load tiles
   */
  void loadTiles(std::shared_ptr<bool> should_run);

  /**
   * @brief Returns an image of the rendererd tile. May return a cached result.
   */
  std::shared_ptr<QImage> renderTile(size_t col, size_t row);

  /** @brief Converts from the world coordinate system to screen coordinates. */
  int64_t worldToScreenX(int64_t world_x) const;

  /** @brief Converts from the world coordinate system to screen coordinates. */
  int64_t worldToScreenY(int64_t world_y) const;

  /** @brief Converts from the world coordinate system to screen coordinates. */
  int64_t screenToWorldX(int64_t world_x) const;

  /** @brief Converts from the world coordinate system to screen coordinates. */
  int64_t screenToWorldY(int64_t world_y) const;

  std::shared_ptr<Map> _map;

  Camera _camera;

  TileRenderer _tile_renderer;

  size_t _num_workers;

  std::shared_ptr<bool> _workers_should_run;
  Semaphore _tiles_to_load;

  Action _action;
  int _last_mouse_x, _last_mouse_y;

  int _tile_size;

  std::unordered_map<size_t, std::shared_ptr<QImage>> _tile_cache;

  Brush _brush;
  size_t _brush_size;
};

}  // namespace atlas
