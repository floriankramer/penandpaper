#pragma once

#include <cstdint>
#include <vector>

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

private:
  size_t _columns;

  // Cell size in meters. The entire map is (_columns - 1) * _cell_size wide
  double _cell_size;

  std::vector<Cell> _cells;
};
}
