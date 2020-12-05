#include "Image.h"

#include <png.h>

namespace atlas {

Image::Image() {}

Image::Image(int64_t width, int64_t height)
    : _width(width), _height(height), _pixels(width * height) {
  ;
}

Image::Image(int64_t width, int64_t height, Pixel color)
    : _width(width), _height(height), _pixels(width * height, color) {}

Image::~Image() {}

void Image::load(const std::string &path) {
  // Based upon https://gist.github.com/niw/5963798
  FILE *fp = fopen(path.c_str(), "r");

  png_structp png =
      png_create_read_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
  if (!png) return;

  png_infop info = png_create_info_struct(png);
  if (!info) return;

  if (setjmp(png_jmpbuf(png))) return;

  png_init_io(png, fp);

  png_read_info(png, info);

  _width = png_get_image_width(png, info);
  _height = png_get_image_height(png, info);
  png_byte color_type = png_get_color_type(png, info);
  png_byte bit_depth = png_get_bit_depth(png, info);

  // Read any color_type into 8bit depth, RGBA format.
  // See http://www.libpng.org/pub/png/libpng-manual.txt

  if (bit_depth == 16) png_set_strip_16(png);

  if (color_type == PNG_COLOR_TYPE_PALETTE) png_set_palette_to_rgb(png);

  // PNG_COLOR_TYPE_GRAY_ALPHA is always 8 or 16bit depth.
  if (color_type == PNG_COLOR_TYPE_GRAY && bit_depth < 8)
    png_set_expand_gray_1_2_4_to_8(png);

  if (png_get_valid(png, info, PNG_INFO_tRNS)) png_set_tRNS_to_alpha(png);

  // These color_type don't have an alpha channel then fill it with 0xff.
  if (color_type == PNG_COLOR_TYPE_RGB || color_type == PNG_COLOR_TYPE_GRAY ||
      color_type == PNG_COLOR_TYPE_PALETTE)
    png_set_filler(png, 0xFF, PNG_FILLER_AFTER);

  if (color_type == PNG_COLOR_TYPE_GRAY ||
      color_type == PNG_COLOR_TYPE_GRAY_ALPHA)
    png_set_gray_to_rgb(png);

  png_read_update_info(png, info);

  png_bytepp rows = (png_bytepp)png_malloc(png, sizeof(png_bytep) * _height);
  for (int64_t y = 0; y < _height; y++) {
    rows[y] = (png_bytep)malloc(png_get_rowbytes(png, info));
  }

  png_read_image(png, rows);

  _pixels.resize(_width * _height);
  for (int64_t y = 0; y < _height; ++y) {
    png_bytep row = rows[y];
    for (int64_t x = 0; x < _width; ++x) {
      Pixel &p = _pixels[y * _width + x];
      p.r = *row++;
      p.g = *row++;
      p.b = *row++;
      p.a = *row++;
    }
  }

  fclose(fp);
  png_destroy_read_struct(&png, &info, NULL);

  for (int y = 0; y < _height; y++) {
    free(rows[y]);
  }
  free(rows);
}

void Image::save(const std::string &path) const {
  FILE *fp = fopen(path.c_str(), "w");

  png_structp png =
      png_create_write_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
  if (!png) return;

  png_infop info = png_create_info_struct(png);
  if (!info) return;

  if (setjmp(png_jmpbuf(png))) return;

  png_init_io(png, fp);

  png_set_IHDR(png, info, _width, _height, 8, PNG_COLOR_TYPE_RGBA,
               PNG_INTERLACE_NONE, PNG_COMPRESSION_TYPE_DEFAULT,
               PNG_FILTER_TYPE_DEFAULT);

  png_write_info(png, info);

  png_bytepp rows = (png_bytepp)png_malloc(png, sizeof(png_bytep) * _height);
  for (int64_t y = 0; y < _height; y++) {
    rows[y] = (png_bytep)malloc(png_get_rowbytes(png, info));
  }

  for (int64_t y = 0; y < _height; ++y) {
    png_bytep row = rows[y];
    for (int64_t x = 0; x < _width; ++x) {
      const Pixel &p = _pixels[y * _width + x];
      *(row++) = p.r;
      *(row++) = p.g;
      *(row++) = p.b;
      *(row++) = p.a;
    }
  }

  png_write_image(png, rows);
  png_write_end(png, info);

  fclose(fp);
  png_destroy_write_struct(&png, &info);

  for (int64_t y = 0; y < _height; y++) {
    free(rows[y]);
  }
  free(rows);
}

Image::Pixel *Image::pixels() { return _pixels.data(); }

const Image::Pixel *Image::pixels() const { return _pixels.data(); }

int64_t Image::width() const { return _width; }
int64_t Image::height() const { return _height; }

void Image::draw(const Image &other, int64_t x, int64_t y) {
  for (int64_t py = 0; py < other._height; ++py) {
    if (y + py >= _height || y + py < 0) {
      continue;
    }
    for (int64_t px = 0; px < other._width; ++px) {
      if (x + px >= _width || x + px < 0) {
        continue;
      }
      Pixel &current = (*this)(px + x, py + y);
      const Pixel &add = other(px, py);
      float one_minus_current_alpha = 1 - (current.a / 255.0);
      float alpha = add.a / 255.0;
      float one_minus_alpha = 1 - alpha;
      current.r = one_minus_alpha * current.r + alpha * add.r;
      current.g = one_minus_alpha * current.g + alpha * add.g;
      current.b = one_minus_alpha * current.b + alpha * add.b;
      current.a = (1 - (one_minus_current_alpha * one_minus_alpha)) * 255;
    }
  }
}

Image Image::subimage(int64_t x, int64_t y, int64_t width, int64_t height,
                      Pixel fill) {
  Image result(width, height, fill);

  width = std::max(int64_t(0), width);
  height = std::max(int64_t(0), height);

  int64_t min_x = std::max(int64_t(0), std::min(x, _width - 1));
  int64_t max_x = std::max(int64_t(0), std::min(x + width, _width - 1));
  int64_t min_y = std::max(int64_t(0), std::min(y, _height - 1));
  int64_t max_y = std::max(int64_t(0), std::min(y + height, _height - 1));

  int64_t result_offset_x = min_x - x;
  int64_t result_offset_y = min_y - y;

  for (int64_t oy = 0; oy < max_y - min_y; oy++) {
    for (int64_t ox = 0; ox < max_x - min_x; ox++) {
      result(result_offset_x + ox, result_offset_y + oy) =
          (*this)(x + ox, y + oy);
    }
  }
  return result;
}

Image::Pixel &Image::operator()(int64_t x, int64_t y) {
  return _pixels[y * _width + x];
}

const Image::Pixel &Image::operator()(int64_t x, int64_t y) const {
  return _pixels[y * _width + x];
}

}  // namespace atlas
