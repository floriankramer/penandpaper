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

#include <nlohmann/json.hpp>

#include "geometry/Vector.h"
#include <cstdint>

class Wall {
 public:
  Wall() {}
  Wall(uint64_t id, const Vector2f &start, const Vector2f &end);

  nlohmann::json toJson() const;

  uint64_t id() const;

  Vector2f &start();
  Vector2f &end();
  bool &isVisible();

 private:
  uint64_t _id;
  Vector2f _start;
  Vector2f _end;
  bool _is_visible;
};
