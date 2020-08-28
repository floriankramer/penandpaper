#pragma once

#include <cstdint>
#include <string>
#include <vector>

#include "OpenSimplexNoise.h"

namespace atlas {
class Map {
 public:
  typedef void (Map::*BrushFunction)(int64_t x, int64_t y, double dist,
                                     int64_t radius);
  struct BoundingBox {
    int64_t min_x, min_y, max_x, max_y;
  };

 private:
  struct Pixel {
    uint8_t altitude;
  };

  struct Transaction {
    bool initialized;
    BoundingBox area;
    std::vector<Pixel> _old_data;
  };

 public:
  Map();
  virtual ~Map();

  void save(const std::string &path);
  void load(const std::string &path);

  /**
   * @brief Initialize the map. Discards previous map data.
   */
  void initialize(size_t width, size_t height, double meter_per_pixel);

  uint8_t &altitude(size_t x, size_t y);
  const uint8_t &altitude(size_t x, size_t y) const;

  BoundingBox softBrushStroke(int64_t start_x, int64_t start_y, int64_t stop_x,
                              int64_t stop_y, int64_t radius, bool add);

  BoundingBox noiseBrushStroke(int64_t start_x, int64_t start_y, int64_t stop_x,
                               int64_t stop_y, int64_t radius);

  BoundingBox smoothBrushStroke(int64_t start_x, int64_t start_y,
                                int64_t stop_x, int64_t stop_y, int64_t radius);

  BoundingBox sharpenBrushStroke(int64_t start_x, int64_t start_y,
                                 int64_t stop_x, int64_t stop_y,
                                 int64_t radius);

  void setBrushStrength(float strength);
  float getBrushStrength() const;

  size_t widthPixels() const;
  size_t heightPixels() const;

  /**
   * @brief Returns the size of the map in meters
   */
  float widthMeters() const;

  /**
   * @brief Returns the height of the map in meters
   */
  float heightMeters() const;

  /**
   * @brief Start recording changes as a transaction. Transactions can be
   * undone.
   */
  void startTransaction();

  /**
   * @brief Stop recording the current transaction and store it on the undo
   * stack.
   */
  void endTransaction();

  /**
   * @brief Undo the top most element of the undo stack.
   */
  BoundingBox undoTransaction();

 private:
  BoundingBox applyBrush(int64_t start_x, int64_t start_y, int64_t stop_x,
                         int64_t stop_y, int64_t radius, BrushFunction brush);

  template <bool positive>
  void softBrush(int64_t x, int64_t y, double dist, int64_t radius);

  void noiseBrush(int64_t x, int64_t y, double dist, int64_t radius);

  void smoothBrush(int64_t x, int64_t y, double dist, int64_t radius);

  void sharpenBrush(int64_t x, int64_t y, double dist, int64_t radius);

  BoundingBox strokeAABB(int64_t start_x, int64_t start_y, int64_t stop_x,
                         int64_t stop_y, int64_t radius) const;

  /**
   * @brief Copies an area out of src into dest. src_area and dest_area must
   * have the same size. The maximums of dest_area are ignored. src_width
   * and dest_width are the width of the src and dest buffer respectively.
   */
  void copyArea(const BoundingBox &src_area, const std::vector<Pixel> &src,
                int64_t src_width, const BoundingBox &dest_area,
                std::vector<Pixel> *dest, int64_t dest_width);

  /**
   * @brief Copies an area out of src into dest. Ignores any pixels inside
   * of area that are also inside of exclude. This function assumes that exclude
   * is contained within area.
   */
  void copyAreaWithout(const BoundingBox &src_area, const BoundingBox &exclude,
                       const std::vector<Pixel> &src, int64_t src_width,
                       const BoundingBox &dest_area, std::vector<Pixel> *dest,
                       int64_t dest_width);

  size_t _width_pixels;
  size_t _height_pixels;
  double _meter_per_pixel;

  std::vector<Pixel> _pixels;

  /** @brief this buffer is used to accumulate transactions. */
  std::vector<Pixel> _undo_pixels;

  OpenSimplexNoise _brush_noise;
  float _brush_strength;

  Transaction _current_transaction;
  std::vector<Transaction> _undo_stack;
};
}  // namespace atlas
