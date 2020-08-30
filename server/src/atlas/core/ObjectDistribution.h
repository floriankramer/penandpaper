#pragma once

#include <array>
#include <cstdint>
#include <vector>

#include "Image.h"

namespace atlas {
class ObjectDistribution {
 public:
  ObjectDistribution(int64_t width, int64_t height, int64_t offset,
                     Image _image);

  void setName(const std::string &name);
  const std::string &name() const;

  const std::vector<std::array<int64_t, 2>> &getPoints() const;
  const Image &img() const;

 private:
  void generate();

  std::string _name;

  int64_t _width;
  int64_t _height;
  int64_t _offset;
  std::vector<std::array<int64_t, 2>> _points;

  Image _image;
};
}  // namespace atlas
