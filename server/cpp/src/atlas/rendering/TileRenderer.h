#pragma once

#include "Image.h"
#include "core/Map.h"

namespace atlas {
class TileRenderer {
 public:
  TileRenderer();
  virtual ~TileRenderer();

  Image renderTile(const Map *map, size_t x, size_t y, size_t zoom,
                   size_t width, size_t height);
};
}  // namespace TileRenderer
