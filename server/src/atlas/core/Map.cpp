#include "Map.h"

#include <cmath>
#include <ctime>
#include <fstream>
#include <iostream>
#include <limits>

#include "../Logger.h"

namespace atlas {
Map::Map() {}
Map::~Map() {}

void Map::initialize(size_t width, size_t height, double meter_per_pixel) {
  _width_pixels = width;
  _height_pixels = height;
  _meter_per_pixel = meter_per_pixel;

  _pixels.resize(width * height);
}

void Map::save(const std::string &path) {
  std::ofstream out(path);
  if (!out.is_open()) {
    throw std::runtime_error("Unable to open: " + path);
  }
  out.write(reinterpret_cast<char *>(&_width_pixels), sizeof(_width_pixels));
  out.write(reinterpret_cast<char *>(&_height_pixels), sizeof(_height_pixels));
  out.write(reinterpret_cast<char *>(&_meter_per_pixel),
            sizeof(_meter_per_pixel));
  out.write(reinterpret_cast<char *>(_pixels.data()),
            sizeof(Pixel) * _pixels.size());
}

void Map::load(const std::string &path) {
  std::ifstream in(path);
  if (!in.is_open()) {
    throw std::runtime_error("Unable to open: " + path);
  }
  in.read(reinterpret_cast<char *>(&_width_pixels), sizeof(_width_pixels));
  in.read(reinterpret_cast<char *>(&_height_pixels), sizeof(_height_pixels));
  in.read(reinterpret_cast<char *>(&_meter_per_pixel),
          sizeof(_meter_per_pixel));
  _pixels.resize(_width_pixels * _height_pixels);
  in.read(reinterpret_cast<char *>(_pixels.data()),
          sizeof(Pixel) * _pixels.size());
}

uint8_t &Map::altitude(size_t x, size_t y) {
  return _pixels[x + y * _width_pixels].altitude;
}

const uint8_t &Map::altitude(size_t x, size_t y) const {
  return _pixels[x + y * _width_pixels].altitude;
}

float Map::widthMeters() const { return _width_pixels * _meter_per_pixel; }

float Map::heightMeters() const { return _height_pixels * _meter_per_pixel; }

size_t Map::widthPixels() const { return _width_pixels; }

size_t Map::heightPixels() const { return _height_pixels; }

Map::BoundingBox Map::softBrushStroke(int64_t start_x, int64_t start_y,
                                      int64_t stop_x, int64_t stop_y,
                                      int64_t radius, bool add) {
  if (add) {
    return applyBrush(start_x, start_y, stop_x, stop_y, radius,
                      &Map::softBrush<true>);
  } else {
    return applyBrush(start_x, start_y, stop_x, stop_y, radius,
                      &Map::softBrush<false>);
  }
}

Map::BoundingBox Map::noiseBrushStroke(int64_t start_x, int64_t start_y,
                                       int64_t stop_x, int64_t stop_y,
                                       int64_t radius) {
  return applyBrush(start_x, start_y, stop_x, stop_y, radius, &Map::noiseBrush);
}

Map::BoundingBox Map::smoothBrushStroke(int64_t start_x, int64_t start_y,
                                        int64_t stop_x, int64_t stop_y,
                                        int64_t radius) {
  return applyBrush(start_x, start_y, stop_x, stop_y, radius,
                    &Map::smoothBrush);
}

Map::BoundingBox Map::strokeAABB(int64_t start_x, int64_t start_y,
                                 int64_t stop_x, int64_t stop_y,
                                 int64_t radius) const {
  BoundingBox aabb;
  if (start_x > stop_x) {
    aabb.min_x = std::max(int64_t(0), stop_x - radius);
    aabb.max_x = std::min(int64_t(_width_pixels), start_x + radius);
  } else {
    aabb.min_x = std::max(int64_t(0), start_x - radius);
    aabb.max_x = std::min(int64_t(_width_pixels), stop_x + radius);
  }
  if (start_y > stop_y) {
    aabb.min_y = std::max(int64_t(0), stop_y - radius);
    aabb.max_y = std::min(int64_t(_height_pixels), start_y + radius);
  } else {
    aabb.min_y = std::max(int64_t(0), start_y - radius);
    aabb.max_y = std::min(int64_t(_height_pixels), stop_y + radius);
  }
  return aabb;
}

Map::BoundingBox Map::applyBrush(
    int64_t start_x, int64_t start_y, int64_t stop_x, int64_t stop_y,
    int64_t radius,
    BrushFunction brush) {  // Compute a bounding box
  BoundingBox aabb = strokeAABB(start_x, start_y, stop_x, stop_y, radius);

  if (stop_x == start_x && stop_y == start_y) {
    // draw a circle
    for (int64_t py = aabb.min_y; py < aabb.max_y; py++) {
      for (int64_t px = aabb.min_x; px < aabb.max_x; px++) {
        // Compute the projected point and the distance from (px, py) to it.
        double dist = hypot(px - start_x, py - start_y);
        if (dist < radius) {
          (this->*brush)(px, py, dist, radius);
        }
      }
    }
  } else {
    // draw a line
    double delta_x = stop_x - start_x;
    double delta_y = stop_y - start_y;
    // Normalize the stroke vector
    double stroke_length = std::hypot(delta_x, delta_y);
    double delta_n_x = delta_x / stroke_length;
    double delta_n_y = delta_y / stroke_length;

    // Iterate all pixels in the bounding box and the cell
    for (int64_t py = aabb.min_y; py < aabb.max_y; py++) {
      for (int64_t px = aabb.min_x; px < aabb.max_x; px++) {
        // compute the brush function at the pixel
        double start_to_p_x = px - start_x;
        double start_to_p_y = py - start_y;
        // Project the start_to_p vector onto the strokes direction vector
        double t = (delta_n_x * start_to_p_x + delta_n_y * start_to_p_y) /
                   stroke_length;
        // Cap the projection to be within the stroke
        t = std::max(0.0, std::min(1.0, t));
        // Compute the projected point and the distance from (px, py) to it.
        double dist =
            hypot(px - (start_x + delta_x * t), py - (start_y + delta_y * t));
        if (dist < radius) {
          // modify the pixel
          (this->*brush)(px, py, dist, radius);
        }
      }
    }
  }
  return aabb;
}

template <bool positive>
void Map::softBrush(int64_t x, int64_t y, double dist, int64_t radius) {
  uint8_t current = _pixels[x + y * _width_pixels].altitude;
  if (positive) {
    dist /= radius;
    _pixels[x + y * _width_pixels].altitude =
        std::min(int(255), int(current + 255 * (1 - dist)));
  } else {
    dist /= radius;
    _pixels[x + y * _width_pixels].altitude =
        std::max(int(0), int(current - 255 * dist));
  }
}
// instantiate both versions
template void Map::softBrush<true>(int64_t x, int64_t y, double dist,
                                   int64_t radius);
template void Map::softBrush<false>(int64_t x, int64_t y, double dist,
                                    int64_t radius);

void Map::noiseBrush(int64_t x, int64_t y, double dist, int64_t radius) {
  double delta = (_brush_noise.noise2(x / 20.0, y / 20.0) - 0.5);
  delta += 0.5 * (_brush_noise.noise2(x / 10.0, y / 10.0) - 0.5);
  delta += 0.25 * (_brush_noise.noise2(x / 5.0, y / 5.0) - 0.5);
  delta *= 16;
  delta *= 1 - (dist / radius);
  uint8_t &current = _pixels[x + y * _width_pixels].altitude;
  current = std::min(255.0, std::max(0.0, current + delta));
}

void Map::smoothBrush(int64_t x, int64_t y, double dist, int64_t radius) {
  uint64_t count = 1;
  float val = _pixels[x + y * _width_pixels].altitude;
  if (x > 0) {
    val += _pixels[(x - 1) + y * _width_pixels].altitude;
    count++;
  }
  if (x < int64_t(_width_pixels)) {
    val += _pixels[(x + 1) + y * _width_pixels].altitude;
    count++;
  }
  if (y > 0) {
    val += _pixels[x + (y - 1) * _width_pixels].altitude;
    count++;
  }
  if (y < int64_t(_height_pixels)) {
    val += _pixels[x + (y + 1) * _width_pixels].altitude;
    count++;
  }
  val /= count;
  _pixels[x + y * _width_pixels].altitude = val;
}

}  // namespace atlas
