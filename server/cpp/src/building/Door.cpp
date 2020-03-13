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
