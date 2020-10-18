#include <libgen.h>
#include <sys/stat.h>
#include <sys/types.h>

#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <nlohmann/json.hpp>
#include <string>

#include "Image.h"

struct Settings {
  std::string input_path;
  std::string output_path;
  double image_width_meters;
};

std::string realpath(const std::string &path) {
  char buffer[4096];
  ::realpath(path.c_str(), buffer);
  return std::string(buffer);
}

std::string dirname(const std::string &dir) {
  char buffer[4096];
  std::strcpy(buffer, dir.c_str());
  return std::string(::dirname(buffer));
}

void mkdir(const std::string &path) {
  int status = ::mkdir(path.c_str(), 0755);
  if (status != 0) {
    if (errno != EEXIST) {
      throw std::runtime_error("Unable to create a directory at " + path + " " +
                               std::string(strerror(errno)));
    }
  }
}

void mkdirs(const std::string &path) {
  std::vector<std::string> paths;
  std::string fullpath = path;
  while (fullpath.size() > 1) {
    paths.push_back(fullpath);
    fullpath = dirname(fullpath);
  }
  for (size_t i = paths.size(); i > 0; i--) {
    mkdir(paths[i - 1]);
  }
}

Settings parseSettings(int argc, char **argv) {
  Settings s;
  if (argc != 4) {
    std::cerr << "Expected exactly three arguments, but got " << (argc - 1)
              << std::endl;
    std::cout << "Usage: " << argv[0]
              << " <input-file> <map-width-meters> <output-folder>"
              << std::endl;
    std::cout << "Takes a png image and converts it to a tilemap." << std::endl;
    exit(1);
  }
  s.input_path = realpath(argv[1]);
  s.image_width_meters = std::stod(argv[2]);
  s.output_path = realpath(argv[3]);

  return s;
}

int main(int argc, char **argv) {
  using nlohmann::json;

  Settings settings = parseSettings(argc, argv);
  atlas::Image img;
  img.load(settings.input_path);

  size_t tiles_x = img.width() / 1024;
  if (img.width() % 1024 != 0) {
    tiles_x++;
  }
  size_t tiles_y = img.height() / 1024;
  if (img.height() % 1024 != 0) {
    tiles_y++;
  }

  std::string zoom_dir = settings.output_path + "/0";
  mkdirs(zoom_dir);

  size_t images_to_create = tiles_x * tiles_y;
  int number_size = std::to_string(images_to_create).size();

  // Generate and write the tiles
  for (size_t y = 0; y < tiles_y; ++y) {
    std::string y_dir = zoom_dir + "/" + std::to_string(y);
    mkdir(y_dir);
    for (size_t x = 0; x < tiles_x; ++x) {
      std::cout << "\rCreating tile [" << std::setw(number_size)
                << (x + y * tiles_x + 1) << "/" << images_to_create << "]";
      fflush(stdout);

      atlas::Image tile = img.subimage(x * 1024, y * 1024, 1024, 1024);
      tile.save(y_dir + "/" + std::to_string(x) + ".png");
    }
  }
  std::cout << "\nDone" << std::endl;

  // write the meta json
  json meta;
  meta["width"] = tiles_x;
  meta["height"] = tiles_y;
  meta["tilesize"] = settings.image_width_meters / (img.width() / 1024.0);

  std::ofstream out(settings.output_path + "/meta.json");
  out << meta.dump(2);
}
