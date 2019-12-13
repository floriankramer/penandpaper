#pragma once

#include <cstdint>
#include <vector>

#include "Biome.h"
#include "Triangle.h"

namespace atlas {
class Image;

class BiomeMap {
 public:
  // The distance between to points on the BiomeMap grid is 500 meters
  static constexpr double RESOLUTION = 500;

  BiomeMap();
  virtual ~BiomeMap();

  void fromImage(const Image &image);

  std::vector<Triangle> tesselate(double x_min, double x_max, double y_min,
                                  double y_max, unsigned int subsampling = 0);

  // The width in meters
  double width() const;
  // The height in meters
  double height() const;

 private:
  uint64_t _width;
  uint64_t _height;
  std::vector<Biome> _data;
};
}  // namespace atlas
