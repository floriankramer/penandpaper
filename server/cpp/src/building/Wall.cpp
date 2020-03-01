#include "Wall.h"

Wall::Wall(uint64_t id, const Vector2f &start, const Vector2f &end)
    : _id(id), _start(start), _end(end), _is_visible(false) {}

nlohmann::json Wall::toJson() const {
  using nlohmann::json;
  json j;
  j["id"] = _id;
  j["start"] = _start.toJson();
  j["end"] = _end.toJson();
  j["is_visible"] = _is_visible;
  return j;
}

uint64_t Wall::id() const { return _id; }

Vector2f &Wall::start() { return _start; }
Vector2f &Wall::end() { return _end; }
bool &Wall::isVisible() { return _is_visible; }
