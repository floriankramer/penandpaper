#pragma once

#include <nlohmann/json.hpp>

#include <cstdint>

#include "geometry/Vector.h"

class Door {
 public:
  Door() {}

  Door(uint64_t id, const Vector2f &position, float width, float rotation);

  uint64_t id() const;

  void setOpen(bool is_open);

  nlohmann::json toJson() const;

  Vector2f &position();
  float &width();
  float &rotation();
  bool &isVisible();
  bool &isOpen();

 private:
  uint64_t _id;
  Vector2f _position;
  float _width;
  float _rotation;
  bool _is_visible;
  bool _is_open;
};
