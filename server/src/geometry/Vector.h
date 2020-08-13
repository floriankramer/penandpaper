#pragma once

#include <nlohmann/json.hpp>

template <typename T>
class Vector2 {
 public:
  Vector2();
  Vector2(T x, T y);

  T length() const;
  void normalize();
  Vector2<T> normalized() const;
  void rotate(T angle);
  Vector2<T> rotated(T angle) const;

  nlohmann::json toJson() const;

  static Vector2<T> fromJson(const nlohmann::json &j);

  T &x();
  const T &x() const;

  T &y();
  const T &y() const;

 private:
  T _x, _y;
};

using Vector2f = Vector2<float>;
using Vector2d = Vector2<double>;
