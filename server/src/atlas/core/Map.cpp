#include "Map.h"

#include <cmath>
#include <ctime>
#include <fstream>
#include <iostream>
#include <limits>

#include "../Logger.h"

std::ostream &operator<<(std::ostream &out,
                         const atlas::Map::BoundingBox &box) {
  out << '(' << box.min_x << ", " << box.min_y << "), (" << box.max_x << ", "
      << box.max_y << ')';
  return out;
}

namespace atlas {
Map::Map() : _brush_strength(1) {}
Map::~Map() {}

void Map::initialize(size_t width, size_t height, double meter_per_pixel) {
  _width_pixels = width;
  _height_pixels = height;
  _meter_per_pixel = meter_per_pixel;

  _pixels.resize(width * height);
  _undo_pixels.resize(_pixels.size());
  _undo_stack.clear();
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

void Map::startTransaction() {
  _current_transaction.initialized = false;

  _current_transaction.area.min_x = 0;
  _current_transaction.area.min_y = 0;
  _current_transaction.area.max_x = 0;
  _current_transaction.area.max_y = 0;

  _current_transaction._old_data.clear();
}

void Map::endTransaction() {
  // Update the current transaction to hold the pixel data from the undo buffer.
  int64_t width =
      _current_transaction.area.max_x - _current_transaction.area.min_x;
  int64_t height =
      _current_transaction.area.max_y - _current_transaction.area.min_y;
  _current_transaction._old_data.resize(width * height);
  copyArea(_current_transaction.area, _undo_pixels, _width_pixels, {0, 0, 0, 0},
           &_current_transaction._old_data, width);
  _undo_stack.emplace_back();
  _undo_stack.back().area = _current_transaction.area;
  std::swap(_undo_stack.back()._old_data, _current_transaction._old_data);
}

Map::BoundingBox Map::undoTransaction() {
  if (_undo_stack.size() > 0) {
    Transaction &trans = _undo_stack.back();
    int64_t width = trans.area.max_x - trans.area.min_x;
    for (size_t i = 0; i < trans._old_data.size(); ++i) {
      int64_t x = trans.area.min_x + (i % width);
      int64_t y = trans.area.min_y + (i / width);
      _pixels[x + y * _width_pixels] = trans._old_data[i];
    }
    BoundingBox changed = trans.area;
    _undo_stack.pop_back();
    return changed;
  } else {
    return {0, 0, 0, 0};
  }
}

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

Map::BoundingBox Map::sharpenBrushStroke(int64_t start_x, int64_t start_y,
                                         int64_t stop_x, int64_t stop_y,
                                         int64_t radius) {
  return applyBrush(start_x, start_y, stop_x, stop_y, radius,
                    &Map::sharpenBrush);
}

void Map::setBrushStrength(float strength) {
  _brush_strength = std::max(0.0f, std::min(1.0f, strength));
}

float Map::getBrushStrength() const { return _brush_strength; }

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

void Map::copyArea(const BoundingBox &src_area, const std::vector<Pixel> &src,
                   int64_t src_width, const BoundingBox &dest_area,
                   std::vector<Pixel> *dest, int64_t dest_width) {
  int64_t width = src_area.max_x - src_area.min_x;
  int64_t height = src_area.max_y - src_area.min_y;
  for (int64_t y = 0; y < height; ++y) {
    int64_t src_y = y + src_area.min_y;
    int64_t dest_y = y + dest_area.min_y;
    for (int64_t x = 0; x < width; ++x) {
      int64_t src_x = x + src_area.min_x;
      int64_t dest_x = x + dest_area.min_x;
      (*dest)[dest_x + dest_y * dest_width] = src[src_x + src_y * src_width];
    }
  }
}

void Map::copyAreaWithout(const BoundingBox &src_area,
                          const BoundingBox &exclude,
                          const std::vector<Pixel> &src, int64_t src_width,
                          const BoundingBox &dest_area,
                          std::vector<Pixel> *dest, int64_t dest_width) {
  // We split src_area into four sub aabbs based upon the area removed by
  // exclude.
  // Those four areas are a bar at the top and bottom, and a box on the left and
  // right side. Not all areas have to exist.
  //
  BoundingBox current_area;
  int64_t width, height;
  int64_t current_offset_x, current_offset_y;

  // Top area
  current_area.min_x = src_area.min_x;
  current_area.min_y = src_area.min_y;
  current_area.max_x = src_area.max_x;
  current_area.max_y = exclude.min_y;
  width = current_area.max_x - current_area.min_x;
  height = current_area.max_y - current_area.min_y;

  for (int64_t y = 0; y < height; ++y) {
    int64_t src_y = y + src_area.min_y;
    int64_t dest_y = y + dest_area.min_y;
    for (int64_t x = 0; x < width; ++x) {
      int64_t src_x = x + src_area.min_x;
      int64_t dest_x = x + dest_area.min_x;
      (*dest)[dest_x + dest_y * dest_width] = src[src_x + src_y * src_width];
    }
  }

  // left area
  current_area.min_x = src_area.min_x;
  current_area.min_y = exclude.min_y;
  current_area.max_x = exclude.min_x;
  current_area.max_y = exclude.max_y;
  width = current_area.max_x - current_area.min_x;
  height = current_area.max_y - current_area.min_y;
  current_offset_y = current_area.min_y - src_area.min_y;
  if (width > 0) {
    for (int64_t y = 0; y < height; ++y) {
      int64_t src_y = y + src_area.min_y + current_offset_y;
      int64_t dest_y = y + dest_area.min_y + current_offset_y;
      for (int64_t x = 0; x < width; ++x) {
        int64_t src_x = x + src_area.min_x;
        int64_t dest_x = x + dest_area.min_x;
        (*dest)[dest_x + dest_y * dest_width] = src[src_x + src_y * src_width];
      }
    }
  }

  // right area
  current_area.min_x = exclude.max_x;
  current_area.min_y = exclude.min_y;
  current_area.max_x = src_area.max_x;
  current_area.max_y = exclude.max_y;
  width = current_area.max_x - current_area.min_x;
  height = current_area.max_y - current_area.min_y;
  current_offset_x = current_area.min_x - src_area.min_x;
  current_offset_y = current_area.min_y - src_area.min_y;

  if (width > 0) {
    for (int64_t y = 0; y < height; ++y) {
      int64_t src_y = y + src_area.min_y + current_offset_y;
      int64_t dest_y = y + dest_area.min_y + current_offset_y;
      for (int64_t x = 0; x < width; ++x) {
        int64_t src_x = x + src_area.min_x + current_offset_x;
        int64_t dest_x = x + dest_area.min_x + current_offset_x;
        (*dest)[dest_x + dest_y * dest_width] = src[src_x + src_y * src_width];
      }
    }
  }

  // Bottom area
  current_area.min_x = src_area.min_x;
  current_area.min_y = exclude.max_y;
  current_area.max_x = src_area.max_x;
  current_area.max_y = src_area.max_y;
  width = current_area.max_x - current_area.min_x;
  height = current_area.max_y - current_area.min_y;
  current_offset_y = current_area.min_y - src_area.min_y;

  for (int64_t y = 0; y < height; ++y) {
    int64_t src_y = y + src_area.min_y + current_offset_y;
    int64_t dest_y = y + dest_area.min_y + current_offset_y;
    for (int64_t x = 0; x < width; ++x) {
      int64_t src_x = x + src_area.min_x;
      int64_t dest_x = x + dest_area.min_x;
      (*dest)[dest_x + dest_y * dest_width] = src[src_x + src_y * src_width];
    }
  }
}

Map::BoundingBox Map::applyBrush(
    int64_t start_x, int64_t start_y, int64_t stop_x, int64_t stop_y,
    int64_t radius,
    BrushFunction brush) {  // Compute a bounding box
  BoundingBox aabb = strokeAABB(start_x, start_y, stop_x, stop_y, radius);

  // update the current transaction
  if (!_current_transaction.initialized) {
    // Initialize the transaction with the data currently contained in the area
    _current_transaction.area = aabb;
    _current_transaction.initialized = true;
    copyArea(aabb, _pixels, _width_pixels, aabb, &_undo_pixels, _width_pixels);
  } else {
    // Grow the transactions area
    BoundingBox new_aabb = _current_transaction.area;
    new_aabb.min_x = std::min(aabb.min_x, new_aabb.min_x);
    new_aabb.min_y = std::min(aabb.min_y, new_aabb.min_y);
    new_aabb.max_x = std::max(aabb.max_x, new_aabb.max_x);
    new_aabb.max_y = std::max(aabb.max_y, new_aabb.max_y);
    // Cop the newly modified area
    copyAreaWithout(new_aabb, _current_transaction.area, _pixels, _width_pixels,
                    new_aabb, &_undo_pixels, _width_pixels);
    _current_transaction.area = new_aabb;
  }

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
        std::min(int(255), int(current + 255 * (1 - dist) * _brush_strength));
  } else {
    dist /= radius;
    _pixels[x + y * _width_pixels].altitude =
        std::max(int(0), int(current - 255 * (1 - dist) * _brush_strength));
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
  delta *= 16 * _brush_strength;
  delta *= 1 - (dist / radius);
  uint8_t &current = _pixels[x + y * _width_pixels].altitude;
  current = std::min(255.0, std::max(0.0, current + delta));
}

void Map::smoothBrush(int64_t x, int64_t y, double dist, int64_t radius) {
  uint64_t count = 0;
  float val = 0;
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
  val *= _brush_strength;

  val += _pixels[x + y * _width_pixels].altitude;

  // normalize
  val /= (_brush_strength * count) + 1;
  _pixels[x + y * _width_pixels].altitude = val;
}

void Map::sharpenBrush(int64_t x, int64_t y, double dist, int64_t radius) {
  uint8_t &current = _pixels[x + y * _width_pixels].altitude;
  float val = (current - 127.0f) / 127.0f;
  val = std::pow(val, _brush_strength * 0.7 + (1 - _brush_strength));
  current = (val + 1) * 127;
}

}  // namespace atlas
