#include "TileRenderer.h"

namespace atlas {
TileRenderer::TileRenderer() {}
TileRenderer::~TileRenderer() {}

Image TileRenderer::renderTile(const Map *map, size_t x, size_t y, size_t zoom,
                               size_t width, size_t height) {
  Image img(width, height, Image::Pixel {64, 128, 0, 255});

  return img;
}

}  // namespace atlas
