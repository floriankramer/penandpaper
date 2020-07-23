#pragma once

#include <cstdint>
#include <vector>

#include "OpenSimplexNoise.h"

namespace atlas {
class Map {
  struct Cell {
    int16_t altitude;
    uint16_t deriv_altitude_x;
    uint16_t deriv_altitude_y;
    uint16_t material_index;
    uint16_t nation_index;
  };
public:
  Map();
  virtual ~Map();

  // columns and rows need to be powers of 2
  void initialize(size_t columns, size_t rows, double cell_size);

  float altitude(double x, double y) const;

  float minAltitude() const;
  float maxAltitude() const;
  float seaLevel() const;

  double cellSize() const;

  size_t rows() const;
  size_t cols() const;

  size_t rowsAtZoom(size_t zoom) const;
  size_t colsAtZoom(size_t zoom) const;

  float width(size_t zoom = 0) const;
  float height(size_t zoom = 0) const;

private:
  double computeAltitude(double x, double y) const;

  double cellAltitude(int64_t x, int64_t y) const;

  OpenSimplexNoise _noise;

  size_t _columns;
  size_t _rows;

  // Cell size in meters. The entire map is (_columns - 1) * _cell_size wide
  double _cell_size;

  std::vector<Cell> _cells;

  float _min_altitude;
  float _max_altitude;
  float _sea_level;
};
}
