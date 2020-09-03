#pragma once

#include <vector>

namespace atlas {
template <class T>
class GridIndex {
  struct Entry {
    T val;
    float min_x, min_y, max_x, max_y;
  };

 public:
  GridIndex(float width, float height, size_t cells_x, size_t cells_y)
      : _width(width),
        _height(height),
        _cell_width(width / cells_x),
        _cell_height(height / cells_y),
        _cells_x(cells_x),
        _cells_y(cells_y) {
    _cells.resize(cells_x * cells_y);
  }

  /**
   * @brief Add an element to the index.
   * @param x The center of the objects bounding box.
   * @param y The center of the objects bounding box.
   * @param width The width of the objects bounding box.
   * @param height The height of the objects bounding box.
   * @param val The object itself.
   */
  void add(float x, float y, float width, float height, const T& val) {
    std::vector<T> vals;
    float min_x = x - width / 2;
    float max_x = x + width / 2;
    float min_y = y - height / 2;
    float max_y = y + height / 2;
    size_t min_cell_x =
        std::min(_cells_x - 1, size_t(std::max(0.0f, min_x / _cell_width)));
    size_t max_cell_x =
        std::min(_cells_x - 1, size_t(std::max(0.0f, max_x / _cell_width))) + 1;
    size_t min_cell_y =
        std::min(_cells_y - 1, size_t(std::max(0.0f, min_y / _cell_height)));
    size_t max_cell_y =
        std::min(_cells_y - 1, size_t(std::max(0.0f, max_y / _cell_height))) +
        1;

    Entry e;
    e.val = val;
    e.min_x = min_x;
    e.min_y = min_y;
    e.max_x = max_x;
    e.max_y = max_y;
    for (size_t cy = min_cell_y; cy < max_cell_y; ++cy) {
      for (size_t cx = min_cell_x; cx < max_cell_x; ++cx) {
        _cells[cx + cy * _cells_y].push_back(e);
      }
    }
  }

  /**
   * @brief Remove an element from the index.
   * @param x The center of the objects bounding box.
   * @param y The center of the objects bounding box.
   * @param width The width of the objects bounding box.
   * @param height The height of the objects bounding box.
   * @param val The object itself.
   */
  void remove(float x, float y, float width, float height, const T& val) {
    std::vector<T> vals;
    float min_x = x - width / 2;
    float max_x = x + width / 2;
    float min_y = y - height / 2;
    float max_y = y + height / 2;
    size_t min_cell_x =
        std::min(_cells_x - 1, size_t(std::max(0.0f, min_x / _cell_width)));
    size_t max_cell_x =
        std::min(_cells_x - 1, size_t(std::max(0.0f, max_x / _cell_width))) + 1;
    size_t min_cell_y =
        std::min(_cells_y - 1, size_t(std::max(0.0f, min_y / _cell_height)));
    size_t max_cell_y =
        std::min(_cells_y - 1, size_t(std::max(0.0f, max_y / _cell_height))) +
        1;

    Entry e;
    e.val = val;
    e.min_x = min_x;
    e.min_y = min_y;
    e.max_x = max_x;
    e.max_y = max_y;
    for (size_t cy = min_cell_y; cy < max_cell_y; ++cy) {
      for (size_t cx = min_cell_x; cx < max_cell_x; ++cx) {
        std::vector<Entry>& v = _cells[cx + cy * _cells_y];
        v.erase(std::remove(v.begin(), v.end(), e), v.end());
      }
    }
  }

  /**
   * @brief Find all objects intersecting the rectangle.
   * @param x The center of the query area's bounding box.
   * @param y The center of the query area's bounding box.
   * @param width The width of the query area's bounding box.
   * @param height The height of the query area's bounding box.
   * @return A list of intersecting objects.
   */
  std::vector<T> queryRect(float x, float y, float width, float height) const {
    std::vector<T> vals;
    float min_x = x - width / 2;
    float max_x = x + width / 2;
    float min_y = y - height / 2;
    float max_y = y + height / 2;
    size_t min_cell_x =
        std::min(_cells_x - 1, size_t(std::max(0.0f, min_x / _cell_width)));
    size_t max_cell_x =
        std::min(_cells_x - 1, size_t(std::max(0.0f, max_x / _cell_width))) + 1;
    size_t min_cell_y =
        std::min(_cells_y - 1, size_t(std::max(0.0f, min_y / _cell_height)));
    size_t max_cell_y =
        std::min(_cells_y - 1, size_t(std::max(0.0f, max_y / _cell_height))) +
        1;

    for (size_t cy = min_cell_y; cy < max_cell_y; ++cy) {
      for (size_t cx = min_cell_x; cx < max_cell_x; ++cx) {
        const std::vector<Entry>& v = _cells[cx + cy * _cells_y];
        for (const Entry& e : v) {
          // check if the rectangles intersect
          if (((e.min_x < min_x && e.max_x > max_x) ||
               (e.min_x < max_x && e.max_x > max_x)) &&
              ((e.min_y < min_y && e.max_y > max_y) ||
               (e.min_y < max_y && e.max_y > max_y))) {
            vals.push_back(e.val);
          }
        }
      }
    }
    return vals;
  }

  /**
   * @brief Find all objects intersecting the rectangle.
   * @param x The center of the query area's bounding box.
   * @param y The center of the query area's bounding box.
   * @param width The width of the query area's bounding box.
   * @param height The height of the query area's bounding box.
   * @return A list of intersecting objects.
   */
  std::vector<const T&> queryRectRef(float x, float y, float width,
                                     float height) const {
    std::vector<T> vals;
    float min_x = x - width / 2;
    float max_x = x + width / 2;
    float min_y = y - height / 2;
    float max_y = y + height / 2;
    size_t min_cell_x =
        std::min(_cells_x - 1, size_t(std::max(0.0f, min_x / _cell_width)));
    size_t max_cell_x =
        std::min(_cells_x - 1, size_t(std::max(0.0f, max_x / _cell_width))) + 1;
    size_t min_cell_y =
        std::min(_cells_y - 1, size_t(std::max(0.0f, min_y / _cell_height)));
    size_t max_cell_y =
        std::min(_cells_y - 1, size_t(std::max(0.0f, max_y / _cell_height))) +
        1;

    for (size_t cy = min_cell_y; cy < max_cell_y; ++cy) {
      for (size_t cx = min_cell_x; cx < max_cell_x; ++cx) {
        const std::vector<Entry>& v = _cells[cx + cy * _cells_y];
        for (const Entry& e : v) {
          // check if the rectangles intersect
          if (((e.min_x < min_x && e.max_x > max_x) ||
               (e.min_x < max_x && e.max_x > max_x)) &&
              ((e.min_y < min_y && e.max_y > max_y) ||
               (e.min_y < max_y && e.max_y > max_y))) {
            vals.push_back(e.val);
          }
        }
      }
    }
    return vals;
  }

  /**
   * @brief Find all objects containing the point.
   * @param x The query point.
   * @param y The query point.
   * @return A list of intersecting objects.
   */
  std::vector<T> query(float x, float y) const {
    std::vector<const T&> vals;
    if (x >= 0 && y >= 0 && x < _width && y < _height) {
      size_t cell_x =
          std::min(_cells_x - 1, size_t(std::max(0.0f, x / _cell_width)));
      size_t cell_y =
          std::min(_cells_y - 1, size_t(std::max(0.0f, y / _cell_height)));

      const std::vector<Entry>& v = _cells[cell_x + cell_y * _cells_y];
      for (const Entry& e : v) {
        if (e.min_x < x && e.max_x > x && e.min_y < y && e.max_y > y) {
          vals.push_back(e.val);
        }
      }
    }
    return vals;
  }

  /**
   * @brief Find all objects containing the point.
   * @param x The query point.
   * @param y The query point.
   * @return A list of intersecting objects.
   */
  std::vector<const T&> queryRef(float x, float y) const {
    std::vector<const T&> vals;
    if (x >= 0 && y >= 0 && x < _width && y < _height) {
      size_t cell_x =
          std::min(_cells_x - 1, size_t(std::max(0.0f, x / _cell_width)));
      size_t cell_y =
          std::min(_cells_y - 1, size_t(std::max(0.0f, y / _cell_height)));

      const std::vector<Entry>& v = _cells[cell_x + cell_y * _cells_y];
      for (const Entry& e : v) {
        if (e.min_x < x && e.max_x > x && e.min_y < y && e.max_y > y) {
          vals.push_back(e.val);
        }
      }
    }
    return vals;
  }

 private:
  std::vector<std::vector<Entry>> _cells;

  float _width;
  float _height;
  float _cell_width;
  float _cell_height;
  size_t _cells_x;
  size_t _cells_y;
};
}  // namespace atlas
