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
  virtual ~Image();

  Pixel &operator()(uint32_t x, uint32_t y);
  const Pixel &operator()(uint32_t x, uint32_t y) const;

  void load(const std::string &path);

  Pixel *pixels();
  const Pixel *pixels() const;

  uint32_t width() const;
  uint32_t height() const;

private:
  uint32_t _width, _height;
  std::vector<Pixel> _pixels;
};
}  // namespace atlas
