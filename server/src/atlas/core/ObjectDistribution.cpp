#include "ObjectDistribution.h"

#include <cstdio>
#include <cstdlib>
#include <ctime>

namespace atlas {

ObjectDistribution::ObjectDistribution(int64_t width, int64_t height,
                                       Image image)
    : _width(width), _height(height), _image(image) {
  generate();
}

const std::vector<std::array<int64_t, 2>> &ObjectDistribution::getPoints()
    const {
  return _points;
}

const Image &ObjectDistribution::img() const { return _image; }

void ObjectDistribution::generate() {
  // Use a grid with offsets to generate the points
  const int64_t offset = 32;
  unsigned int seed = time(NULL);
  _points.reserve((_width / offset) * (_height / offset));

  for (int64_t y = 0; y < _height / offset; ++y) {
    for (int64_t x = 0; x < _width / offset; ++x) {
      int64_t pos_x = x * offset + offset / 2;
      int64_t pos_y = y * offset + offset / 2;
      pos_x += (rand_r(&seed) / double(RAND_MAX) - 0.5) * offset;
      pos_y += (rand_r(&seed) / double(RAND_MAX) - 0.5) * offset;
      _points.push_back({pos_x, pos_y});
    }
  }
  fflush(stdout);
}

}  // namespace atlas
