#include "Door.h"

Door::Door(uint64_t id, const Vector2f &position, float width, float rotation)
    : _id(id),
      _position(position),
      _width(width),
      _rotation(rotation),
      _is_visible(false),
      _is_open(false) {}

uint64_t Door::id() const { return _id; }

Vector2f &Door::position() { return _position; }
float &Door::width() { return _width; }
float &Door::rotation() { return _rotation; }
bool &Door::isVisible() { return _is_visible; }
bool &Door::isOpen() { return _is_open; }

void Door::setOpen(bool is_open) { _is_open = is_open; }

nlohmann::json Door::toJson() const {
  using nlohmann::json;
  json j;
  j["id"] = _id;
  j["position"] = _position.toJson();
  j["width"] = _width;
  j["rotation"] = _rotation;
  j["is_visible"] = _is_visible;
  j["is_open"] = _is_open;
  return j;
}
