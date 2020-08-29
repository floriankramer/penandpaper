#pragma once

#include "core/Image.h"
#include "core/Map.h"

namespace atlas {
class TileRenderer {
 public:
  TileRenderer();
  virtual ~TileRenderer();

  Image renderTile(const Map *map, size_t min_x, size_t min_y, size_t max_x,
                   size_t max_y);
};
}  // namespace atlas
