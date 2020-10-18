#pragma once

#include <string>
#include <vector>

namespace atlas {

class Image {
 public:
  struct Pixel {
    uint8_t r, g, b, a;
  };

  Image();
  Image(int64_t width, int64_t height);
  Image(int64_t width, int64_t height, Pixel color);
  virtual ~Image();

  Pixel &operator()(int64_t x, int64_t y);
  const Pixel &operator()(int64_t x, int64_t y) const;

  void load(const std::string &path);
  void save(const std::string &path) const;

  Pixel *pixels();
  const Pixel *pixels() const;

  int64_t width() const;
  int64_t height() const;

  void draw(const Image &other, int64_t x, int64_t y);

  Image subimage(int64_t x, int64_t y, int64_t width, int64_t height,
                 Pixel fill = {0, 0, 0, 0});

 private:
  int64_t _width, _height;
  std::vector<Pixel> _pixels;
};
}  // namespace atlas
