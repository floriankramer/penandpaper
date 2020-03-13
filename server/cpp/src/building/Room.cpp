/**
 * Copyright 2020 Florian Kramer
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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
