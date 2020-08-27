#pragma once

#include <cstdint>
#include <string>
#include <vector>

#include "OpenSimplexNoise.h"

namespace atlas {
class Map {
 public:
  typedef void (Map::*BrushFunction)(int64_t x, int64_t y, double dist,
                                     int64_t radius);
  struct BoundingBox {
    int64_t min_x, min_y, max_x, max_y;
  };

 private:
  struct Pixel {
    uint8_t altitude;
    uint16_t material_index;
    uint16_t nation_index;
  };

 public:
  Map();
  virtual ~Map();

  void save(const std::string &path);
  void load(const std::string &path);

  /**
   * @brief Initialize the map. Discards previous map data.
   */
  void initialize(size_t width, size_t height, double meter_per_pixel);

  uint8_t &altitude(size_t x, size_t y);
  const uint8_t &altitude(size_t x, size_t y) const;

  BoundingBox softBrushStroke(int64_t start_x, int64_t start_y, int64_t stop_x,
                              int64_t stop_y, int64_t radius, bool add);

  BoundingBox noiseBrushStroke(int64_t start_x, int64_t start_y, int64_t stop_x,
                               int64_t stop_y, int64_t radius);

  BoundingBox smoothBrushStroke(int64_t start_x, int64_t start_y,
                                int64_t stop_x, int64_t stop_y, int64_t radius);

  size_t widthPixels() const;
  size_t heightPixels() const;

  /**
   * @brief Returns the size of the map in meters
   */
  float widthMeters() const;

  /**
   * @brief Returns the height of the map in meters
   */
  float heightMeters() const;

 private:
  BoundingBox applyBrush(int64_t start_x, int64_t start_y, int64_t stop_x,
                         int64_t stop_y, int64_t radius, BrushFunction brush);

  template <bool positive>
  void softBrush(int64_t x, int64_t y, double dist, int64_t radius);

  void noiseBrush(int64_t x, int64_t y, double dist, int64_t radius);

  void smoothBrush(int64_t x, int64_t y, double dist, int64_t radius);

  BoundingBox strokeAABB(int64_t start_x, int64_t start_y, int64_t stop_x,
                         int64_t stop_y, int64_t radius) const;

  size_t _width_pixels;
  size_t _height_pixels;
  double _meter_per_pixel;

  std::vector<Pixel> _pixels;

  OpenSimplexNoise _brush_noise;
};
}  // namespace atlas
