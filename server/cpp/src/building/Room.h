#pragma once

#include <cstdint>
#include <vector>

#include <nlohmann/json.hpp>

#include "geometry/Vector.h"
#include "Furniture.h"

class Room {
public:
  Room() { }

  Room(uint64_t id, const Vector2f &position, const Vector2f &size);

  nlohmann::json toJson() const;

  bool contains(const Vector2f &pos) const;

  uint64_t id() const;

  Vector2f &position();
  Vector2f &size();
  bool &isVisible();

private:
  uint64_t _id;
  Vector2f _position;
  Vector2f _size;
  bool _is_visible;
};
