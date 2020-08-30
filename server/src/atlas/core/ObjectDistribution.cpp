#include "ObjectDistribution.h"

#include <algorithm>
#include <cstdio>
#include <cstdlib>
#include <ctime>

namespace atlas {

ObjectDistribution::ObjectDistribution(int64_t width, int64_t height,
                                       int64_t offset, Image image)
    : _width(width), _height(height), _offset(offset), _image(image) {
  generate();
}

void ObjectDistribution::setName(const std::string &name) { _name = name; }

const std::string &ObjectDistribution::name() const { return _name; }

const std::vector<std::array<int64_t, 2>> &ObjectDistribution::getPoints()
    const {
  return _points;
}

const Image &ObjectDistribution::img() const { return _image; }

void ObjectDistribution::generate() {
  using Point = std::array<int64_t, 2>;
  // Use a grid with offsets to generate the points
  unsigned int seed = time(NULL);
  _points.reserve((_width / _offset) * (_height / _offset));

  for (int64_t y = 0; y < _height / _offset; ++y) {
    for (int64_t x = 0; x < _width / _offset; ++x) {
      int64_t pos_x = x * _offset + _offset / 2;
      int64_t pos_y = y * _offset + _offset / 2;
      pos_x += (rand_r(&seed) / double(RAND_MAX) - 0.5) * 0.5 * _offset;
      pos_y += (rand_r(&seed) / double(RAND_MAX) - 0.5) * 0.5 * _offset;
      _points.push_back({pos_x, pos_y});
    }
  }
  // sort the points by ascending y position
  std::sort(
      _points.begin(), _points.end(),
      [](const Point &p1, const Point &p2) -> bool { return p1[1] < p2[1]; });
}

}  // namespace atlas
