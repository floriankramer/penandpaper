#pragma once

#include <cstdint>
#include <vector>

#include "Biome.h"
#include "Triangle.h"

namespace atlas {
class Image;

class BiomeMap {
 public:
  struct BiomeTriangle {
    Triangle t;
    Biome b;
  };

  // The distance between to points on the BiomeMap grid is 500 meters
  static constexpr double RESOLUTION = 500;

  BiomeMap();
  virtual ~BiomeMap();

  void fromImage(const Image &image);

  std::vector<BiomeTriangle> tesselate(double x_min, double x_max, double y_min,
                                       double y_max,
                                       unsigned int subsampling = 0);

  // The width in meters
  double width() const;
  // The height in meters
  double height() const;

  Biome at(double x, double y);
  Biome atIdx(uint64_t x, uint64_t y);

 private:
  uint64_t _grid_width;
  uint64_t _grid_height;
  std::vector<Biome> _data;
};
}  // namespace atlas
