#pragma once

#include <nlohmann/json.hpp>

#include "geometry/Vector.h"
#include <cstdint>

class Wall {
 public:
  Wall() {}
  Wall(uint64_t id, const Vector2f &start, const Vector2f &end);

  nlohmann::json toJson() const;

  uint64_t id() const;

  Vector2f &start();
  Vector2f &end();
  bool &isVisible();

 private:
  uint64_t _id;
  Vector2f _start;
  Vector2f _end;
  bool _is_visible;
};
