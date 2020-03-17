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

#include "Doodad.h"

DoodadLine::DoodadLine(IdGenerator *idg) : _id(idg->next()), _sx(0), _sy(0), _ex(0), _ey(0) {}

nlohmann::json DoodadLine::serialize() {
  using nlohmann::json;
  json j;
  j["id"] = _id;
  j["type"] = "line";
  j["sx"] = _sx;
  j["sy"] = _sy;
  j["ex"] = _ex;
  j["ey"] = _ey;
  return j;
}

void DoodadLine::deserialize(const nlohmann::json &j) {
  if (j.find("id") != j.end()) {
    _id = j.at("id").get<uint64_t>();
  }
  _sx = j.at("sx").get<float>();
  _sy = j.at("sy").get<float>();
  _ex = j.at("ex").get<float>();
  _ey = j.at("ey").get<float>();
}
