#include "BiomeMap.h"

#include "../Logger.h"
#include "Image.h"

namespace atlas {
BiomeMap::BiomeMap() {}
BiomeMap::~BiomeMap() {}

void BiomeMap::fromImage(const Image &image) {
  _data.clear();
  _grid_width = image.width();
  _grid_height = image.height();
  LOG_INFO << "Creating a biome map of size " << _grid_width << " x "
           << _grid_height << LOG_END;

  _data.resize(_grid_width * _grid_height, Biome::VOID);
  for (uint64_t i = 0; i < _grid_width * _grid_height; ++i) {
    uint64_t x = i % _grid_width;
    uint64_t y = i / _grid_height;
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

std::vector<BiomeMap::BiomeTriangle> BiomeMap::tesselate(
    double x_min, double x_max, double y_min, double y_max,
    unsigned int subsampling) {
  std::vector<BiomeMap::BiomeTriangle> triangles;
  if (subsampling > 0) {
    LOG_ERROR << "Subsampling is not yet supported." << LOG_END;
    throw std::runtime_error("Subsampling is not yet supported.");
  }

  if (_grid_width == 0 || _grid_height == 0) {
    LOG_ERROR << "Tried to tesselate an empty map." << LOG_END;
    throw std::runtime_error("Tried to tesselate an empty map.");
  }

  // find grid coordinates enclosing the area
  uint64_t x_min_i = x_min / width() * _grid_width;
  uint64_t x_max_i = x_max / width() * _grid_width;
  uint64_t y_min_i = y_min / height() * _grid_height;
  uint64_t y_max_i = y_max / height() * _grid_height;
  x_max_i = std::min(x_max_i, _grid_width - 1);
  x_min_i = std::min(x_min_i, x_max_i);
  y_max_i = std::min(y_max_i, _grid_height - 1);
  y_min_i = std::min(y_min_i, y_max_i);

  // Run Marching squares on the grid for every biome
  for (Biome b = Biome::GRASSLAND; b <= Biome::SEA; b = Biome(int(b) + 1)) {
    for (uint64_t x = x_min_i; x + 1 < x_max_i; ++x) {
      for (uint64_t y = y_min_i; y + 1 < y_max_i; ++y) {
        uint_fast8_t sq_idx = 0;
        if (atIdx(x, y) == b) {
          sq_idx += 1;
        }
        if (atIdx(x + 1, y) == b) {
          sq_idx += 1 << 1;
        }
        if (atIdx(x + 1, y + 1) == b) {
          sq_idx += 1 << 2;
        }
        if (atIdx(x, y + 1) == b) {
          sq_idx += 1 << 3;
        }
        switch (sq_idx) {
          case 15: {
            // The area is full
            BiomeTriangle t1;
            t1.b = b;
            t1.t.p1.x = x;
            t1.t.p1.y = y;

            t1.t.p1.x = x;
            t1.t.p1.y = y + 1;

            t1.t.p1.x = x + 1;
            t1.t.p1.y = y;
            triangles.push_back(t1);

            BiomeTriangle t2;
            t2.b = b;
            t2.t.p1.x = x + 1;
            t2.t.p1.y = y;

            t2.t.p1.x = x;
            t2.t.p1.y = y + 1;

            t2.t.p1.x = x + 1;
            t2.t.p1.y = y + 1;
            triangles.push_back(t2);
          } break;
          case 14: {
            // The top left is missing
            BiomeTriangle t1;
            t1.b = b;
            t1.t.p1.x = x;
            t1.t.p1.y = (y + y + 1) / 2.0;

            t1.t.p1.x = x;
            t1.t.p1.y = y + 1;

            t1.t.p1.x = x + 1;
            t1.t.p1.y = y + 1;
            triangles.push_back(t1);

            BiomeTriangle t2;
            t2.b = b;
            t2.t.p1.x = (x + x + 1) / 2.0;
            t2.t.p1.y = y;

            t2.t.p1.x = x + 1;
            t2.t.p1.y = y + 1;

            t2.t.p1.x = x + 1;
            t2.t.p1.y = y;
            triangles.push_back(t2);

            BiomeTriangle t3;
            t3.b = b;
            t3.t.p1.x = (x + x + 1) / 2.0;
            t3.t.p1.y = y;

            t3.t.p1.x = x;
            t3.t.p1.y = (y + y + 1) / 2.0;

            t3.t.p1.x = x + 1;
            t3.t.p1.y = y + 1;
            triangles.push_back(t3);
          } break;
          case 13: {
            // Everything but the top right

          } break;
        }
      }
    }
  }

  return triangles;
}

Biome BiomeMap::at(double x, double y) {
  if (x < 0 || y < 0) {
    throw std::runtime_error(
        "Tried to access a value outside of the biome map.");
  }
  uint64_t x_i = x / width() * _grid_width;
  uint64_t y_i = y / height() * _grid_height;
  return atIdx(x_i, y_i);
}

Biome BiomeMap::atIdx(uint64_t x, uint64_t y) {
  if (x >= _grid_width || y >= _grid_height) {
    throw std::runtime_error(
        "Tried to access a value outside of the biome map.");
  }
  return _data[x + y * _grid_width];
}

double BiomeMap::width() const { return RESOLUTION * (_grid_width - 1); }

double BiomeMap::height() const { return RESOLUTION * (_grid_height - 1); }
}  // namespace atlas
