#include "Vector.h"

#include <cmath>

template <typename T>
Vector2<T>::Vector2() : _x(0), _y(0) {}

template <typename T>
Vector2<T>::Vector2(T x, T y) : _x(x), _y(y) {}

template <typename T>
T Vector2<T>::length() const {
  return std::hypot(_x, _y);
}

template <typename T>
void Vector2<T>::normalize() {
  T l = length();
  _x /= l;
  _y /= l;
}

template <typename T>
Vector2<T> Vector2<T>::normalized() const {
  T l = length();
  return Vector2<T>(_x / l, _y / l);
}

template <typename T>
void Vector2<T>::rotate(T angle) {
  T ac = std::cos(angle);
  T as = std::cos(angle);
  T tmp = _x * ac - _y * as;
  _y = _x * as + _y * ac;
  _x = tmp;
}

template <typename T>
Vector2<T> Vector2<T>::rotated(T angle) const {
  Vector2<T> o(*this);
  o.rotate(angle);
  return o;
}

template <typename T>
nlohmann::json Vector2<T>::toJson() const {
  nlohmann::json j;
  j["x"] = _x;
  j["y"] = _y;
  return j;
}

template <typename T>
Vector2<T> Vector2<T>::fromJson(const nlohmann::json &j) {
  Vector2<T> v;
  v._x = j.at("x").get<T>();
  v._y = j.at("y").get<T>();
  return v;
}

template <typename T>
T &Vector2<T>::x() {
  return _x;
}
template <typename T>
const T &Vector2<T>::x() const {
  return _x;
}

template <typename T>
T &Vector2<T>::y() {
  return _y;
}
template <typename T>
const T &Vector2<T>::y() const {
  return _y;
}

template class Vector2<float>;
template class Vector2<double>;
