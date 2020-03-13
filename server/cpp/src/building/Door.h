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

#include <cstdint>

#include "geometry/Vector.h"

class Door {
 public:
  Door() {}

  Door(uint64_t id, const Vector2f &position, float width, float rotation);

  uint64_t id() const;

  void setOpen(bool is_open);

  nlohmann::json toJson() const;

  Vector2f &position();
  float &width();
  float &rotation();
  bool &isVisible();
  bool &isOpen();

 private:
  uint64_t _id;
  Vector2f _position;
  float _width;
  float _rotation;
  bool _is_visible;
  bool _is_open;
};
