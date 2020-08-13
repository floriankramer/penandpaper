#include <ft2build.h>
#include FT_FREETYPE_H

#include <msdfgen/msdfgen-ext.h>
#include <msdfgen/msdfgen.h>
#include <png.h>

#include <cassert>
#include <cmath>
#include <fstream>
#include <iostream>
#include <nlohmann/json.hpp>
#include <sstream>

const unsigned char BACKGROUND = 255;

class Glyph {
 public:
  nlohmann::json toJson() const {
    using nlohmann::json;
    json j;
    j["character"] = std::string(&_character, 1);
    j["width"] = _width;
    j["height"] = _height;
    j["advance"] = _advance;
    j["bearingX"] = _bearing_x;
    j["bearingY"] = _bearing_y;
    j["uMin"] = _u_min;
    j["uMax"] = _u_max;
    j["vMin"] = _v_min;
    j["vMax"] = _v_max;
    return j;
  }

  unsigned char &operator()(size_t i) { return _data[i]; }
  const unsigned char &operator()(size_t i) const { return _data[i]; }

  unsigned char &operator()(size_t x, size_t y) {
    return _data[x + y * _px_width];
  }
  const unsigned char &operator()(size_t x, size_t y) const {
    return _data[x + y * _px_width];
  }

  size_t size() const { return _data.size(); }

  char _character;  // the character this glyph represents

  std::vector<unsigned char> _data;
  size_t _px_width;
  size_t _px_height;

  float _width;
  float _height;
  float _advance;
  // The distance from the origin to the left edge of the glyph
  float _bearing_x;
  // The distance from the origin to the top of the glyph
  float _bearing_y;

  float _u_min;
  float _u_max;
  float _v_min;
  float _v_max;
};

class MsdfGlyph {
 public:
  nlohmann::json toJson() const {
    using nlohmann::json;
    json j;
    j["character"] = std::string(&_character, 1);
    j["width"] = _width;
    j["height"] = _height;
    j["advance"] = _advance;
    j["bearingX"] = _bearing_x;
    j["bearingY"] = _bearing_y;
    j["uMin"] = _u_min;
    j["uMax"] = _u_max;
    j["vMin"] = _v_min;
    j["vMax"] = _v_max;
    return j;
  }

  char _character;  // the character this glyph represents

  msdfgen::Bitmap<float, 3> _data;
  int _px_width;
  int _px_height;

  float _width;
  float _height;
  float _advance;
  // The distance from the origin to the left edge of the glyph
  float _bearing_x;
  // The distance from the origin to the top of the glyph
  float _bearing_y;

  float _u_min;
  float _u_max;
  float _v_min;
  float _v_max;
};

void printUsage(char *exename) {
  std::cout << "Usage: " << exename << " <input.ttf> <output_prefix>"
            << std::endl;
}

std::string getRequiredCharacters() {
  std::stringstream s;
  for (char c = 32; c <= 126; ++c) {
    s << c;
  }
  return s.str();
}

void glyphAddMargin(Glyph *g, int margin) {
  //
  std::vector<unsigned char> ndata;
  size_t nwidth = g->_px_width + 2 * margin;
  size_t nheight = g->_px_height + 2 * margin;
  ndata.resize(nwidth * nheight, BACKGROUND);

  // copy the old glyph to the center of the new one
  for (size_t x = 0; x < g->_px_width; ++x) {
    for (size_t y = 0; y < g->_px_height; ++y) {
      ndata[x + margin + (y + margin) * nwidth] = (*g)(x, y);
    }
  }

  // update the metrics
  // The size of a pixel in the glyph's metric's coordinate system
  float ds = g->_px_width / g->_width;
  // To preserve the rendered size the actual size of the glyph has to increase
  g->_width += 2 * margin * ds;
  g->_height += 2 * margin * ds;
  // The bearings need to be updates to ensure the position of the visual part
  // of the glyph relative to the origin stays the same.
  g->_bearing_x -= margin * ds;
  g->_bearing_y += margin * ds;
  // The advance doesn't change (the visible size of the glyph doesn't change).

  std::swap(ndata, g->_data);
  g->_px_width += 2 * margin;
  g->_px_height += 2 * margin;
}

void glyphHalfBitmap(Glyph *g) {
  std::vector<unsigned char> ndata;
  size_t nwidth = g->_px_width / 2;
  size_t nheight = g->_px_height / 2;
  ndata.resize(nwidth * nheight, BACKGROUND);
  for (size_t x = 0; x < nwidth; ++x) {
    for (size_t y = 0; y < nheight; ++y) {
      ndata[x + y * nwidth] =
          0.25 * (*g)(x * 2, y * 2) + 0.25 * (*g)(x * 2 + 1, y * 2) +
          0.25 * (*g)(x * 2, y * 2 + 1) + 0.25 * (*g)(x * 2 + 1, y * 2 + 1);
    }
  }
  std::swap(ndata, g->_data);
  g->_px_width /= 2;
  g->_px_height /= 2;
}

/**
 * @brief dtf
 * @param data
 * @param stride The distance between two neighbouring elements
 * @param size The number of elements (data has to have size * stride entries)
 */
void dtf(unsigned char *data, size_t stride, size_t size) {
  // Some part of this implementation is still broken

  // The distance at which the squared distance value is 255
  const float FALLOFF = 4;
  const float SCALE = sqrt(255) / FALLOFF;

  // Based upon
  // Felzenszwalb, Pedro F., and Daniel P. Huttenlocher.
  // "Distance transforms of sampled functions."
  // Theory of computing 8.1 (2012): 415-428.
  //
  // Can be found at https://cs.brown.edu/people/pfelzens/papers/dt-final.pdf

  // The rough idea is to consider a parabola positioned on every pixel and
  // offset upwards by the value at that pixel. Then we compute the lower
  // envelope of those parabolas by looking at parabola intersections.

  // Create a read only copy of the data
  std::vector<float> r(size);
  for (size_t i = 0; i < size; ++i) {
    r[i] = data[i * stride];
  }

  // Index of the rightmost parabola in the envelope
  size_t k = 0;
  // locations of parabolas in the lower envelope
  std::vector<ssize_t> v(size, 0);
  // locations of boundaries between pbs
  std::vector<ssize_t> z(size + 1, 0);
  z[0] = 0;
  z[1] = size;
  // Compute the lower envelope of parabolas positioned at each position
  // and offset upwards by data[pos]
  // Consider every parabola in the dataset
  for (ssize_t q = 1; q < size; ++q) {
    while (true) {
      // Intersect the parabolas at q and k
      ssize_t s = std::round(((r[q] + (q * q)) - (r[v[k]] + (v[k] * v[k]))) /
                             double(2 * q - 2 * v[k]));

      if (s < z[k] && k > 0) {
        // The intersection is to the left of the left intersection of the
        // parabola k. That implies that the parabola k is not a part of the
        // lower envelope, but instead covered by the parabol at q (not
        // entirely, but for the purposes of the lower envelope)
        k--;
      } else if (s < z[k] && k == 0) {
        // The intersection is to the left of the first parabola. Replace it
        // with the parabola at q.
        v[k] = q;
        z[k] = -1;
        z[k + 1] = size;
        break;
      } else {
        // The parabola q forms the lower envelope from the intersection point
        // s onwards.
        k++;
        v[k] = q;
        z[k] = s;
        z[k + 1] = size;
        break;
      }
    }
  }
  // Write the distances to data
  k = 0;
  for (size_t q = 0; q < size; ++q) {
    while (z[k + 1] < q) {
      k++;
    }
    float f = (q - v[k]) * (q - v[k]) + r[v[k]];
    // TODO: the mapping with the stride works, but the results are rather
    // random (they look very blurred
    data[q * stride] = std::round(std::min(255.0f, std::max(0.0f, f)));
  }
}

void edtf(unsigned char *data, size_t stride, size_t size, size_t falloff) {
  // The distance at which the squared distance value is 255
  float scale = sqrt(255) / falloff;

  std::vector<float> tmp(size);

  float max = 0;
  for (ssize_t q = 0; q < tmp.size(); ++q) {
    float min = std::numeric_limits<float>::max();
    for (ssize_t i = 0; i < size; ++i) {
      float d = (i - q) * (i - q) * scale * scale + data[i * stride];
      min = std::min(d, min);
    }
    tmp[q] = min;
    max = std::max(max, min);
  }

  for (size_t q = 0; q < tmp.size(); ++q) {
    data[q * stride] = tmp[q] / max * 255;
  }
}

void glyphToSdf(Glyph *g, size_t falloff) {
  // vertical transform
  for (size_t col = 0; col < g->_px_width; ++col) {
    edtf(g->_data.data() + col, g->_px_width, g->_px_height, falloff);
  }
  // horizontal transform
  for (size_t row = 0; row < g->_px_height; ++row) {
    edtf(g->_data.data() + row * g->_px_width, 1, g->_px_width, falloff);
  }
}

std::vector<float> mergeGlyphs(std::vector<Glyph> &glyphs, size_t border) {
  // Simply determine the largest extend of a glyph in both dimensions and
  // then create a grid of square cells based upon that.
  size_t max_width = 0;
  size_t max_height = 0;
  for (const Glyph &g : glyphs) {
    max_width = std::max(max_width, g._px_width);
    max_height = std::max(max_height, g._px_height);
  }
  size_t grid_size = std::ceil(std::sqrt(glyphs.size()));
  size_t cell_size = std::max(max_width, max_height) + 2 * border;
  std::cout << glyphs.size() << " < " << cell_size * cell_size << std::endl;
  assert(glyphs.size() < cell_size * cell_size);
  size_t img_size = grid_size * cell_size;
  // Find the next power of 2
  img_size = std::round(std::pow(2, std::ceil(std::log2(img_size))));
  std::cout << "Producing an image of size " << img_size << std::endl;
  std::vector<float> data(img_size * img_size, BACKGROUND);
  size_t col = 0;
  size_t row = 0;
  for (Glyph &g : glyphs) {
    for (size_t x = 0; x < g._px_width; ++x) {
      for (size_t y = 0; y < g._px_height; ++y) {
        size_t tx = col * cell_size + border + x;
        size_t ty = row * cell_size + border + y;
        data[tx + ty * img_size] = g(x, y);
      }
    }
    g._u_min = double(col * cell_size + border) / img_size;
    g._u_max = double(col * cell_size + border + g._px_width) / img_size;
    g._v_min = double(row * cell_size + border) / img_size;
    g._v_max = double(row * cell_size + border + g._px_height) / img_size;

    col++;
    if (col >= grid_size) {
      row++;
      col = 0;
    }
  }
  return data;
}

std::vector<std::array<unsigned char, 3>> mergeGlyphsMsdf(
    std::vector<MsdfGlyph> &glyphs, size_t border) {
  // Simply determine the largest extend of a glyph in both dimensions and
  // then create a grid of square cells based upon that.
  int max_width = 0;
  int max_height = 0;
  for (const MsdfGlyph &g : glyphs) {
    max_width = std::max(max_width, g._data.width());
    max_height = std::max(max_height, g._data.height());
  }
  size_t grid_size = std::ceil(std::sqrt(glyphs.size()));
  size_t cell_size = std::max(max_width, max_height) + 2 * border;
  std::cout << glyphs.size() << " < " << cell_size * cell_size << std::endl;
  assert(glyphs.size() < cell_size * cell_size);
  size_t img_size = grid_size * cell_size;
  // Find the next power of 2
  img_size = std::round(std::pow(2, std::ceil(std::log2(img_size))));
  std::cout << "Producing an image of size " << img_size << std::endl;

  std::vector<std::array<unsigned char, 3>> data(img_size * img_size);
  for (std::array<unsigned char, 3> &a : data) {
    a[0] = 0;
    a[1] = 0;
    a[2] = 0;
  }

  size_t col = 0;
  size_t row = 0;
  for (MsdfGlyph &g : glyphs) {
    for (size_t x = 0; x < g._data.width(); ++x) {
      for (size_t y = 0; y < g._data.height(); ++y) {
        size_t tx = col * cell_size + border + x;
        size_t ty = row * cell_size + border + y;
        data[tx + ty * img_size][0] = std::max(
            0.0f,
            std::min(255.0f, g._data(x, g._data.height() - 1 - y)[0] * 255));
        data[tx + ty * img_size][1] = std::max(
            0.0f,
            std::min(255.0f, g._data(x, g._data.height() - 1 - y)[1] * 255));
        data[tx + ty * img_size][2] = std::max(
            0.0f,
            std::min(255.0f, g._data(x, g._data.height() - 1 - y)[2] * 255));
      }
    }
    //    g._u_min = double(col * cell_size + border) / img_size;
    //    g._u_max = double(col * cell_size + border + g._px_width) / img_size;
    //    g._v_min = double(row * cell_size + border) / img_size;
    //    g._v_max = double(row * cell_size + border + g._px_height) / img_size;
    g._u_min = double(col * cell_size + border) / img_size;
    g._u_max = double(col * cell_size + border + g._data.width()) / img_size;
    g._v_min = double(row * cell_size + border) / img_size;
    g._v_max = double(row * cell_size + border + g._data.height()) / img_size;

    col++;
    if (col >= grid_size) {
      row++;
      col = 0;
    }
  }
  return data;
}

void saveAsPng(const std::vector<std::array<unsigned char, 3>> &data,
               size_t width, size_t height, const std::string &path) {
  // Based upon <https://gist.github.com/niw/5963798

  FILE *fp = fopen(path.c_str(), "wb");
  if (!fp) {
    std::cerr << "unable to open " << path << std::endl;
    return;
  }

  png_structp png =
      png_create_write_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
  if (!png) {
    std::cout << "Unable to create a png write struct" << std::endl;
    fclose(fp);
    return;
  }

  png_infop info = png_create_info_struct(png);
  if (!info) {
    std::cout << "unable to create a png info struct" << std::endl;
    fclose(fp);
    png_destroy_write_struct(&png, &info);
    return;
  }

  if (setjmp(png_jmpbuf(png))) {
    std::cout << "Unable to setup the png jmpbuf" << std::endl;
    fclose(fp);
    png_destroy_write_struct(&png, &info);
  }

  png_init_io(png, fp);

  // Output is 8bit depth, RGBA format.
  png_set_IHDR(png, info, width, height, 8, PNG_COLOR_TYPE_RGBA,
               PNG_INTERLACE_NONE, PNG_COMPRESSION_TYPE_DEFAULT,
               PNG_FILTER_TYPE_DEFAULT);
  png_write_info(png, info);

  // init the png data
  png_bytepp rows = new png_bytep[height];
  for (size_t row = 0; row < height; ++row) {
    rows[row] = new png_byte[width * 4];
    for (size_t col = 0; col < width; ++col) {
      rows[row][col * 4 + 0] = data[col + row * height][0];
      rows[row][col * 4 + 1] = data[col + row * height][1];
      rows[row][col * 4 + 2] = data[col + row * height][2];
      rows[row][col * 4 + 3] = 255;
    }
  }

  png_write_image(png, rows);
  png_write_end(png, NULL);

  for (size_t row = 0; row < height; ++row) {
    delete[] rows[row];
  }
  delete[] rows;

  fclose(fp);

  png_destroy_write_struct(&png, &info);
}

void saveFloatsAsPng(const std::vector<float> &data, size_t width,
                     size_t height, const std::string &path) {
  // Based upon <https://gist.github.com/niw/5963798

  FILE *fp = fopen(path.c_str(), "wb");
  if (!fp) {
    std::cerr << "unable to open " << path << std::endl;
    return;
  }

  png_structp png =
      png_create_write_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
  if (!png) {
    std::cout << "Unable to create a png write struct" << std::endl;
    fclose(fp);
    return;
  }

  png_infop info = png_create_info_struct(png);
  if (!info) {
    std::cout << "unable to create a png info struct" << std::endl;
    fclose(fp);
    png_destroy_write_struct(&png, &info);
    return;
  }

  if (setjmp(png_jmpbuf(png))) {
    std::cout << "Unable to setup the png jmpbuf" << std::endl;
    fclose(fp);
    png_destroy_write_struct(&png, &info);
  }

  png_init_io(png, fp);

  // Output is 8bit depth, RGBA format.
  png_set_IHDR(png, info, width, height, 8, PNG_COLOR_TYPE_RGBA,
               PNG_INTERLACE_NONE, PNG_COMPRESSION_TYPE_DEFAULT,
               PNG_FILTER_TYPE_DEFAULT);
  png_write_info(png, info);

  // init the png data
  png_bytepp rows = new png_bytep[height];
  for (size_t row = 0; row < height; ++row) {
    rows[row] = new png_byte[width * 4];
    for (size_t col = 0; col < width; ++col) {
      rows[row][col * 4 + 0] = data[col + row * height];
      rows[row][col * 4 + 1] = data[col + row * height];
      rows[row][col * 4 + 2] = data[col + row * height];
      rows[row][col * 4 + 3] = 255;
    }
  }

  png_write_image(png, rows);
  png_write_end(png, NULL);

  for (size_t row = 0; row < height; ++row) {
    delete[] rows[row];
  }
  delete[] rows;

  fclose(fp);

  png_destroy_write_struct(&png, &info);
}

std::array<int, 4> determinePixelBoundsMsdf(MsdfGlyph *g) {
  int x_min = g->_data.width();
  int x_max = 0;
  int y_min = g->_data.height();
  int y_max = 0;
  for (int x = 0; x < g->_data.width(); ++x) {
    for (int y = 0; y < g->_data.height(); ++y) {
      int iy = g->_data.height() - 1 - y;
      float *val = g->_data(x, iy);
      // take the median value
      float d = std::max(std::min(val[0], val[1]),
                         std::min(val[2], std::max(val[0], val[1])));
      if (d >= 0.5) {
        x_min = std::min(x_min, x);
        x_max = std::max(x_max, x);
        y_min = std::min(y_min, iy);
        y_max = std::max(y_max, iy);
      }
    }
  }
  return {x_min, y_min, x_max, y_max};
}

void recomputeMetricsMsdf(MsdfGlyph *g) {
  std::array<int, 4> pixel_bounds = determinePixelBoundsMsdf(g);

  float rel_height = g->_px_height / 24.0;
  float px_to_pt = 1024 / g->_data.width();

  // adjust the bearing
  // g->_bearing_x -= pixel_bounds[0] * px_to_pt;
  g->_bearing_x = 0;
  g->_bearing_y = g->_data.height() *
                  px_to_pt;  // The bearing_y is rendered into the texture

  // We render the full texture for all glyphs
  g->_height /= rel_height;
  g->_width = g->_height;
  if (pixel_bounds[2] > pixel_bounds[0]) {
    g->_advance = (pixel_bounds[2] - pixel_bounds[0]) * px_to_pt;
  } else {
    g->_advance = 20 * px_to_pt;
  }

  // TODO: this is an experiment
  g->_bearing_x = 0;
  g->_bearing_y = 1024;  // The bearing_y is rendered into the texture

  // We render the full texture for all glyphs
  g->_height = 1024;
  g->_width = 1024;
  if (pixel_bounds[2] > pixel_bounds[0]) {
    g->_advance = (pixel_bounds[2] - pixel_bounds[0]) * px_to_pt + 120;
  } else {
    g->_advance = 256;
  }
}

template <typename T>
nlohmann::json glyphsToJson(const std::vector<T> &glyphs) {
  using nlohmann::json;
  json root;
  for (const T &g : glyphs) {
    root.push_back(g.toJson());
  }
  return root;
}

template <typename T>
void saveGlyphData(const std::vector<T> &glyphs, const std::string &path) {
  std::ofstream out(path);
  if (!out.is_open()) {
    std::cerr << "Unable to write to " << path << std::endl;
    return;
  }
  out << glyphsToJson(glyphs).dump(2, ' ');
}

void renderFont(const std::string &inpath, const std::string &outpath) {
  const int MARGIN = 4;  // Area added around a glypf for the sdf
  const int BORDER = 1;  // Area added around the border that is set to 0
  const int TILE_SIZE = 24;
  const int SSP_POWER = 3;
  const int SUPERSAMPLING = 1 << SSP_POWER;
  const size_t FALLOFF = 6;

  std::string required_characters = getRequiredCharacters();

  // Init freetype and load the file
  FT_Library library;
  FT_Error fterror = FT_Init_FreeType(&library);
  if (fterror) {
    std::cerr << "Unable to initialize freetype, exiting due to " << fterror
              << std::endl;
    exit(1);
  }
  FT_Face ft_face;
  fterror = FT_New_Face(library, inpath.c_str(), 0, &ft_face);
  if (fterror) {
    std::cerr << "Unable to load the font from " << inpath << " due to "
              << fterror << std::endl;
  }
  // 24 pt on a 120 dpi device
  // fterror = FT_Set_Char_Size(ft_face, 0, 24 * 64, 120, 120);
  fterror = FT_Set_Pixel_Sizes(ft_face, 0, TILE_SIZE * SUPERSAMPLING);

  std::vector<Glyph> glyphs;
  // Render the glyphs and read out their metrics
  for (char c : required_characters) {
    glyphs.emplace_back();
    Glyph &g = glyphs.back();
    g._character = c;

    FT_UInt cidx = FT_Get_Char_Index(ft_face, c);
    fterror = FT_Load_Glyph(ft_face, cidx, FT_LOAD_DEFAULT);
    if (fterror) {
      std::cerr << "Unable to load the glyph for character " << c << std::endl;
      continue;
    }
    // Ensure we have a bitmap version of the glyph by rendering it to a bitmap
    // if it is in another format.
    if (ft_face->glyph->format != FT_GLYPH_FORMAT_BITMAP) {
      fterror = FT_Render_Glyph(ft_face->glyph, FT_RENDER_MODE_NORMAL);
      if (fterror) {
        std::cerr << "Unable to render the glyph for characer " << c
                  << std::endl;
      }
    }
    FT_GlyphSlot fg = ft_face->glyph;

    // copy the rendered glyph data
    g._data.resize(fg->bitmap.width * fg->bitmap.rows);
    g._px_width = fg->bitmap.width;
    g._px_height = fg->bitmap.rows;
    for (size_t i = 0; i < g.size(); ++i) {
      g(i) = 255 - fg->bitmap.buffer[i];
    }
    g._width = fg->metrics.width;
    g._height = fg->metrics.height;
    g._advance = fg->metrics.horiAdvance;
    g._bearing_x = fg->metrics.horiBearingX;
    g._bearing_y = fg->metrics.horiBearingY;
  }

  // Transform the glyphs to sdfs
  for (Glyph &g : glyphs) {
    glyphAddMargin(&g, MARGIN * SUPERSAMPLING);
    glyphToSdf(&g, FALLOFF * SUPERSAMPLING);
    for (size_t i = 0; i < SSP_POWER; ++i) {
      glyphHalfBitmap(&g);
    }
  }

  // Merge the glyphs to a single image
  std::vector<float> img_data = mergeGlyphs(glyphs, BORDER);
  size_t img_size = std::round(std::sqrt(img_data.size()));

  // save the image as a png
  saveFloatsAsPng(img_data, img_size, img_size, outpath + ".png");

  // save the glyph data as json
  saveGlyphData(glyphs, outpath + ".json");
}

void renderFontMsdfgen(const std::string &inpath, const std::string &outpath) {
  const int MARGIN = 4;  // Area added around a glypf for the sdf
  const int BORDER = 1;  // Area added around the border that is set to 0
  const int TILE_SIZE = 32;

  std::string required_characters = getRequiredCharacters();

  // Init freetype and load the file
  FT_Library library;
  FT_Error fterror = FT_Init_FreeType(&library);
  if (fterror) {
    std::cerr << "Unable to initialize freetype, exiting due to " << fterror
              << std::endl;
    exit(1);
  }
  FT_Face ft_face;
  fterror = FT_New_Face(library, inpath.c_str(), 0, &ft_face);
  if (fterror) {
    std::cerr << "Unable to load the font from " << inpath << " due to "
              << fterror << std::endl;
  }
  // 24 pt on a 120 dpi device
  // fterror = FT_Set_Char_Size(ft_face, 0, 24 * 64, 120, 120);
  fterror = FT_Set_Pixel_Sizes(ft_face, 0, TILE_SIZE - 2 * MARGIN);

  std::vector<MsdfGlyph> glyphs;
  // Load the glyph metrics
  for (char c : required_characters) {
    glyphs.emplace_back();
    MsdfGlyph &g = glyphs.back();
    g._character = c;

    FT_UInt cidx = FT_Get_Char_Index(ft_face, c);
    fterror = FT_Load_Glyph(ft_face, cidx, FT_LOAD_DEFAULT);
    if (fterror) {
      std::cerr << "Unable to load the glyph for character " << c << std::endl;
      continue;
    }
    // Ensure we have a bitmap version of the glyph by rendering it to a bitmap
    // if it is in another format.
    if (ft_face->glyph->format != FT_GLYPH_FORMAT_BITMAP) {
      fterror = FT_Render_Glyph(ft_face->glyph, FT_RENDER_MODE_NORMAL);
      if (fterror) {
        std::cerr << "Unable to render the glyph for characer " << c
                  << std::endl;
      }
    }
    FT_GlyphSlot fg = ft_face->glyph;

    g._px_width = fg->bitmap.width;
    g._px_height = fg->bitmap.rows;
    g._width = fg->metrics.width;
    g._height = fg->metrics.height;
    g._advance = fg->metrics.horiAdvance;
    g._bearing_x = fg->metrics.horiBearingX;
    g._bearing_y = fg->metrics.horiBearingY;
  }

  // Render the glyphs
  msdfgen::FreetypeHandle *ft = msdfgen::initializeFreetype();
  if (!ft) {
    throw std::runtime_error("Unable to create a msdfgen freetype handle");
  }
  msdfgen::FontHandle *msdf_font = msdfgen::loadFont(ft, inpath.c_str());
  size_t gpos = 0;
  for (char c : required_characters) {
    msdfgen::Shape shape;
    if (msdfgen::loadGlyph(shape, msdf_font, c)) {
      MsdfGlyph &g = glyphs[gpos];
      g._data = msdfgen::Bitmap<float, 3>(32, 32);

      shape.normalize();
      msdfgen::edgeColoringSimple(shape, 3.0);
      // The translation offsets the shape by 4 pixels into the image
      msdfgen::generateMSDF(g._data, shape, 4.0,
                            double(TILE_SIZE - 2 * MARGIN) / TILE_SIZE,
                            msdfgen::Vector2(MARGIN, MARGIN));

      recomputeMetricsMsdf(&g);
    }
    gpos++;
  }
  msdfgen::destroyFont(msdf_font);

  // Merge the glyphs to a single image
  std::vector<std::array<unsigned char, 3>> img_data =
      mergeGlyphsMsdf(glyphs, BORDER);
  size_t img_size = std::round(std::sqrt(img_data.size()));

  // save the image as a png
  saveAsPng(img_data, img_size, img_size, outpath + ".png");

  // save the glyph data as json
  saveGlyphData(glyphs, outpath + ".json");
}

int main(int argc, char **argv) {
  if (argc != 3) {
    std::cout << "Expected exactly three arguments" << std::endl;
    printUsage(argv[0]);
    return 1;
  }
  std::string inpath(argv[1]);
  std::string outpath(argv[2]);
  // Load the ttf file, render the required glyphs and convert them
  // to a signed distance field, then pack them onto a single power of two
  // image.
  renderFontMsdfgen(inpath, outpath);
  return 0;
}
