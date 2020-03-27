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
  static Room fromJson(const nlohmann::json &j, uint64_t id);

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
