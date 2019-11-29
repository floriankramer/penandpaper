#include "Doodad.h"

DoodadLine::DoodadLine() : _id(0), _sx(0), _sy(0), _ex(0), _ey(0) {}

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
  } else {
    _id = 0;
  }
  _sx = j.at("sx").get<float>();
  _sy = j.at("sy").get<float>();
  _ex = j.at("ex").get<float>();
  _ey = j.at("ey").get<float>();
}
