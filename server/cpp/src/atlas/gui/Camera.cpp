#include "Camera.h"

namespace atlas {
Camera::Camera() : _x(0), _y(0), _height(1), _aspect_ratio(1) {}

void Camera::setPosition(double x, double y) {
  _x = x;
  _y = y;
}

void Camera::setHeight(double height) { _height = height; }

void Camera::setAspectRatio(double aspect_ratio) {
  _aspect_ratio = aspect_ratio;
}

double Camera::x() const { return _x; }

double Camera::y() const { return _y; }

double Camera::height() const { return _height; }

double Camera::aspectRatio() const { return _aspect_ratio; }

double Camera::width() const { return _height * _aspect_ratio; }

void Camera::move(double x, double y) {
  _x += x;
  _y += y;
}

void Camera::zoom(double factor) { _height *= factor; }
}  // namespace atlas
