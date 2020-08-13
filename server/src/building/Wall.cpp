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

Wall Wall::fromJson(const nlohmann::json &j, uint64_t id) {
  Vector2f start = Vector2f::fromJson(j["start"]);
  Vector2f end = Vector2f::fromJson(j["end"]);
  bool is_visible = j["is_visible"].get<bool>();
  Wall r(id, start, end);
  r._is_visible = is_visible;
  return r;
}

uint64_t Wall::id() const { return _id; }

Vector2f &Wall::start() { return _start; }
Vector2f &Wall::end() { return _end; }
bool &Wall::isVisible() { return _is_visible; }
