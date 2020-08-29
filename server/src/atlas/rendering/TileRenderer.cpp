#include "TileRenderer.h"

#include <iostream>

namespace atlas {
TileRenderer::TileRenderer() {}
TileRenderer::~TileRenderer() {}

Image TileRenderer::renderTile(const Map *map, size_t min_x, size_t min_y,
                               size_t max_x, size_t max_y) {
  constexpr int FEATHER = 10;
  size_t width = max_x - min_x;
  size_t height = max_y - min_y;
  Image img(width, height);
#pragma omp parallel for
  for (int64_t i = 0; i < int64_t(width * height); ++i) {
    size_t px = i % width;
    size_t py = i / width;

    uint8_t altitude = map->altitude(min_x + px, min_y + py);
    Image::Pixel color;
    if (altitude < 127 - FEATHER) {
      color.r = 0x1a;
      color.g = 0x1d;
      color.b = 0x63;
      color.a = 255;
    } else if (altitude > 127 + FEATHER) {
      color.r = 0xd2;
      color.g = 0xc0;
      color.b = 0x9e;
      color.a = 255;
    } else {
      // interp the two
      float interp = float(altitude - (127 - FEATHER)) / (2 * FEATHER);
      color.r = (1 - interp) * 0x1a + interp * 0xd2;
      color.g = (1 - interp) * 0x1d + interp * 0xc0;
      color.b = (1 - interp) * 0x63 + interp * 0x9e;
      color.a = 255;
    }

    img(px, py) = color;
  }

  for (const ObjectDistribution &dist : map->objectDistributions()) {
    for (const std::array<int64_t, 2> &point : dist.getPoints()) {
      img.draw(dist.img(), point[0] - min_x - dist.img().width() / 2,
               point[1] - min_y - dist.img().height() / 2);
    }
  }

  // img.save("/tmp/tile.png");
  return img;
}

}  // namespace atlas
