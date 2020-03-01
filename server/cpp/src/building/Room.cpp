#include "Room.h"

#include "Util.h"

Room::Room(uint64_t id, const Vector2f &position, const Vector2f &size)
    : _id(id), _position(position), _size(size), _is_visible(false) {}

nlohmann::json Room::toJson() const {
  using nlohmann::json;
  json j;
  j["id"] = _id;
  j["position"] = _position.toJson();
  j["size"] = _size.toJson();
  j["is_visible"] = _is_visible;
  return j;
}

bool Room::contains(const Vector2f &pos) const {
  return pos.x() > _position.x() - _size.x() / 2 &&
         pos.x() < _position.y() + _size.y() / 2 &&
         pos.y() > _position.y() - _size.y() / 2 &&
         pos.y() < _position.y() + _size.y() / 2;
}

Vector2f &Room::position() { return _position; }
Vector2f &Room::size() { return _size; }
bool &Room::isVisible() { return _is_visible; }

uint64_t Room::id() const { return _id; }
