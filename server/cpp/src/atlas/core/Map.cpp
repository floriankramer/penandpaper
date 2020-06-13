#include "Map.h"

#include <ctime>

#include "OpenSimplexNoise.h"

namespace atlas {
Map::Map() {}
Map::~Map() {}

void Map::initialize(size_t columns, size_t rows, double cell_size) {
  _columns = columns;
  _cells.resize(columns * rows);
  _cell_size = cell_size;

  // Initialize the altitude values from noise
  OpenSimplexNoise noise(time(NULL));

  for (size_t i = 0; i < _cells.size(); ++i) {
    size_t row = i % rows;
    size_t col = i / columns;
    _cells[i].altitude = (noise.noise2(col * _cell_size, row * cell_size) * 1.4 - 0.4) * 2000;
  }
}
}  // namespace atlas
