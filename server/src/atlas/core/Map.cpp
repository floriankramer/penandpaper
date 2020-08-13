#include "Map.h"

#include <cmath>
#include <ctime>
#include <iostream>
#include <limits>

namespace atlas {
Map::Map() : _sea_level(0) {}
Map::~Map() {}

void Map::initialize(size_t columns, size_t rows, double cell_size) {
  _columns = columns;
  _rows = rows;
  _cells.resize(columns * rows);
  _cell_size = cell_size;

  _min_altitude = std::numeric_limits<float>::max();
  _max_altitude = std::numeric_limits<float>::lowest();

#pragma omp parallel for
  for (size_t i = 0; i < _cells.size(); ++i) {
    size_t row = i % rows;
    size_t col = i / columns;
    float cell_altitude = computeAltitude(col * _cell_size, row * cell_size);
    _cells[i].altitude = cell_altitude;
    _min_altitude = std::min(_min_altitude, cell_altitude);
    _max_altitude = std::max(_max_altitude, cell_altitude);
  }
}

double Map::computeAltitude(double x, double y) const {
  double noise = (_noise.noise2(x / 4000, y / 4000));
  noise += (_noise.noise2(x / 2000, y / 2000) - 0.5) * 0.5;
  noise += (_noise.noise2(x / 500, y / 500) - 0.5) * 0.25;

  double clamp = std::max(
      0.0, 1 - (std::hypot(x - width() / 2, y - height() / 2) / (width() / 2)));
  clamp = std::pow(clamp, 0.3);
  return (clamp * noise * 1.4 - 0.4) * 2000 + clamp * 4000;
}

float Map::altitude(double x, double y) const {
  // Project the altitude on the closest grid cell
  double cell_x_d = x / _cell_size;
  double cell_y_d = y / _cell_size;
  int64_t cell_x = std::floor(cell_x_d);
  int64_t cell_y = std::floor(cell_y_d);
  cell_x = std::min(int64_t(_columns - 1), std::max(int64_t(0), cell_x));
  cell_y = std::min(int64_t(_rows - 1), std::max(int64_t(0), cell_y));

  double interp_x = cell_x_d - std::floor(cell_x_d);
  double interp_y = cell_y_d - std::floor(cell_y_d);

  // interpolate the top
  double top = (1 - interp_x) * cellAltitude(cell_x, cell_y) +
               interp_x * cellAltitude(cell_x + 1, cell_y);
  double bot = (1 - interp_x) * cellAltitude(cell_x, cell_y + 1) +
               interp_x * cellAltitude(cell_x + 1, cell_y + 1);

  double interp = (1 - interp_y) * top + interp_y * bot;
  return interp;
}

double Map::cellAltitude(int64_t x, int64_t y) const {
  x = std::min(int64_t(_columns - 1), std::max(int64_t(0), x));
  y = std::min(int64_t(_rows - 1), std::max(int64_t(0), y));
  return _cells[x + y * _columns].altitude;
}

float Map::minAltitude() const { return _min_altitude; }

float Map::maxAltitude() const { return _max_altitude; }

float Map::seaLevel() const { return _sea_level; }

size_t Map::rows() const { return _rows; }

size_t Map::cols() const { return _columns; }

size_t Map::rowsAtZoom(size_t zoom) const {
  return std::min(_rows, (size_t(1) << zoom));
}

size_t Map::colsAtZoom(size_t zoom) const {
  return std::min(_columns, (size_t(1) << zoom));
}

double Map::cellSize() const { return _cell_size; }

float Map::width(size_t zoom) const {
  return (_columns * _cell_size) / (1 << zoom);
}

float Map::height(size_t zoom) const {
  return (_rows * _cell_size) / (1 << zoom);
}

}  // namespace atlas
