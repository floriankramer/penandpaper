#pragma once

#include <nlohmann/json.hpp>

#include "geometry/Vector.h"
#include <cstdint>

class Furniture {
public:
  Furniture() { }

  Furniture(uint64_t id, Vector2f position, Vector2f size, float rotation);

  nlohmann::json toJson() const;

  uint64_t id() const;
  Vector2f &position();
  Vector2f &size();
  float &rotation();
  bool &isVisible();

private:
  uint64_t _id;
  Vector2f _position;
  Vector2f _size;
  float _rotation;
  bool _is_visible;
};
