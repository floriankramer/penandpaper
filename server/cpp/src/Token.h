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
