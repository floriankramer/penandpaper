#pragma once

#include <cstdint>
#include <array>
#include <vector>

#include "Image.h"

namespace atlas {
class ObjectDistribution {
 public:
  ObjectDistribution(int64_t width, int64_t height, Image _image);

  const std::vector<std::array<int64_t, 2>> &getPoints() const;
  const Image &img() const;

 private:
  void generate();

  int64_t _width;
  int64_t _height;
  std::vector<std::array<int64_t, 2>> _points;

  Image _image;
};
}  // namespace atlas
