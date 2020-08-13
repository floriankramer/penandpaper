#pragma once

namespace atlas {
class Camera {
public:
  Camera();

  // setters
  void setPosition(double x, double y);
  void setHeight(double height);
  void setAspectRatio(double aspect_ratio);

  // getters
  double x() const;
  double y() const;
  double height() const;
  double aspectRatio() const;

  // computed properties
  double width() const;

  /**
   * @brief Moves the center of the visible area by the given amount
   */
  void move(double x, double y);

  /**
   * @brief Changes the size of the visible area by factor. A factor < 1 reduces
   *        the visible area, effectively zooming in.
   */
  void zoom(double factor);

private:
  /** @brief The position in world space */
  double _x, _y;

  /** @brief The height of the visible area */
  double _height;

  /** @brief The width of the visible area divided by its height */
  double _aspect_ratio;
};
}
