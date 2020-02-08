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

#include "Serializeable.h"

class Token : public Serializable {
 public:
  Token();

  virtual nlohmann::json serialize() override;
  virtual void deserialize(const nlohmann::json &json) override;

  uint64_t &id() { return _id; }
  float &x() { return _x; }
  float &y() { return _y; }
  float &radius() { return _radius; }
  float &r() { return _r; }
  float &g() { return _g; }
  float &b() { return _b; }
  bool &is_enemy() { return _is_enemy; }

 private:
  uint64_t _id;
  float _x, _y;
  float _radius;
  float _r, _g, _b;
  bool _is_enemy;
};
