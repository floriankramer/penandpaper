#include "TileRenderer.h"

#include <iostream>

namespace atlas {
TileRenderer::TileRenderer() {}
TileRenderer::~TileRenderer() {}

Image TileRenderer::renderTile(const Map *map, size_t x, size_t y, size_t zoom,
                               size_t width, size_t height) {
  Image img(width, height);
#pragma omp parallel for
  for (int64_t i = 0; i < int64_t(width * height); ++i) {
    size_t px = i % width;
    size_t py = i / width;

    double pos_x = x * map->width(zoom) + double(px) / width * map->width(zoom);
    double pos_y =
        y * map->height(zoom) + double(py) / height * map->height(zoom);

    float altitude = map->altitude(pos_x, pos_y);
    Image::Pixel color;
    if (altitude < map->seaLevel()) {
      color.r = 0x1b;
      color.g = 0x78;
      color.b = 0xaf;
      color.a = 255;
    } else {
      uint8_t c = (altitude - map->minAltitude()) /
                      (map->maxAltitude() - map->minAltitude()) * 255;
      color.r = c;
      color.g = c;
      color.b = c;
      color.a = 255;
    }

    img(px, py) = color;
  }
  img.save("/tmp/tile.png");
  return img;
}

}  // namespace atlas
