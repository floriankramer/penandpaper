#include "BiomeMap.h"

#include "../Logger.h"
#include "Image.h"

namespace atlas {
BiomeMap::BiomeMap() {}
BiomeMap::~BiomeMap() {}

void BiomeMap::fromImage(const Image &image) {
  _data.clear();
  _width = image.width();
  _height = image.height();
  LOG_INFO << "Creating a biome map of size " << _width << " x " << _height
           << LOG_END;

  _data.resize(_width * _height, Biome::VOID);
  for (uint64_t i = 0; i < _width * _height; ++i) {
    uint64_t x = i % _width;
    uint64_t y = i / _height;
    const Image::Pixel &p = image(x, y);
    if (p.r == 0 && p.g == 0 && p.b == 0) {
      _data[i] = Biome::MOUNTAIN;
    } else if (p.r == 0 && p.g == 255 && p.b == 0) {
      _data[i] = Biome::GRASSLAND;
    } else if (p.r == 0 && p.g == 128 && p.b == 0) {
      _data[i] = Biome::WOODS;
    } else if (p.r == 0 && p.g == 0 && p.b == 255) {
      _data[i] = Biome::LAKE;
    } else if (p.r == 0 && p.g == 0 && p.b == 128) {
      _data[i] = Biome::SEA;
    }
  }
}

std::vector<Triangle> BiomeMap::tesselate(double x_min, double x_max,
                                          double y_min, double y_max,
                                          unsigned int subsampling) {
  if (subsampling > 0) {
    LOG_ERROR << "Subsampling is not yet supported." << LOG_END;
    throw std::runtime_error("Subsampling is not yet supported.");
  }


}

double BiomeMap::width() const { return RESOLUTION * (_width - 1); }

double BiomeMap::height() const { return RESOLUTION * (_height - 1); }
}  // namespace atlas
