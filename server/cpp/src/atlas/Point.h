#pragma once
#include <cmath>

namespace atlas {
class Point {
public:
  double length() {
    return std::hypot(x, y);
  }

  double x, y;
};
}
