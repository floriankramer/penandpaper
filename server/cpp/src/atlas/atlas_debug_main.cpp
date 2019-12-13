#include <iostream>
#include <string>

#include "Atlas.h"
#include "BiomeMap.h"
#include "Image.h"

int main(int argc, char **argv) {
  using namespace atlas;
  if (argc != 2) {
    std::cout << "Usage: " << argv[0] << " <image.png>" << std::endl;
    return 1;
  }

  Image img;
  img.load(std::string(argv[1]));

  BiomeMap biome_map;
  biome_map.fromImage(img);
}
