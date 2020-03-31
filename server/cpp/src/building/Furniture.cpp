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

#include "Furniture.h"

Furniture::Furniture(uint64_t id, Vector2f position, Vector2f size,
                     float rotation)
    : _id(id),
      _position(position),
      _size(size),
      _rotation(rotation),
      _is_visible(false) {}

nlohmann::json Furniture::toJson() const {
  nlohmann::json j;
  j["id"] = _id;
  j["position"] = _position.toJson();
  j["size"] = _size.toJson();
  j["rotation"] = _rotation;
  j["is_visible"] = _is_visible;
  return j;
}

Furniture Furniture::fromJson(const nlohmann::json &j, uint64_t id) {
  Vector2f position = Vector2f::fromJson(j["position"]);
  Vector2f size = Vector2f::fromJson(j["size"]);
  bool is_visible = j["is_visible"].get<bool>();
  float rotation = j["rotation"].get<float>();
  Furniture r(id, position, size, rotation);
  r._is_visible = is_visible;
  return r;
}

uint64_t Furniture::id() const { return _id; }

Vector2f &Furniture::position() { return _position; }
Vector2f &Furniture::size() { return _size; }
float &Furniture::rotation() { return _rotation; }
bool &Furniture::isVisible() { return _is_visible; }
