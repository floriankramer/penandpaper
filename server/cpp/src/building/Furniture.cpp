#include "Furniture.h"

Furniture::Furniture(uint64_t id, Vector2f position, Vector2f size,
                     float rotation)
    : _id(id), _position(position), _size(size), _is_visible(false) {}

nlohmann::json Furniture::toJson() const {
  nlohmann::json j;
  j["id"] = _id;
  j["position"] = _position.toJson();
  j["size"] = _size.toJson();
  j["rotation"] = _rotation;
  j["is_visible"] = _is_visible;
  return j;
}

uint64_t Furniture::id() const { return _id; }

Vector2f &Furniture::position() { return _position; }
Vector2f &Furniture::size() { return _size; }
float &Furniture::rotation() { return _rotation; }
bool &Furniture::isVisible() { return _is_visible; }
