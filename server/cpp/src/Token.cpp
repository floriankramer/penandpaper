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

#include "Token.h"

Token::Token()
    : _id(0), _x(0), _y(0), _radius(0), _r(1), _g(1), _b(1), _is_enemy(false) {}

nlohmann::json Token::serialize() {
  using nlohmann::json;
  json j;
  j["id"] = _id;
  j["x"] = _x;
  j["y"] = _y;
  j["radius"] = _radius;
  j["r"] = _r;
  j["g"] = _g;
  j["b"] = _b;
  j["foe"] = _is_enemy;
  return j;
}

void Token::deserialize(const nlohmann::json &j) {
  _id = j["id"].get<uint64_t>();
  _x = j["x"].get<float>();
  _y = j["y"].get<float>();
  _radius = j["radius"].get<float>();
  _r = j["r"].get<float>();
  _g = j["g"].get<float>();
  _b = j["b"].get<float>();
  _is_enemy = j["foe"].get<bool>();
}
