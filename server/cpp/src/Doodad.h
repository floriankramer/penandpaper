#pragma once

#include <cstdint>

#include "Serializeable.h"

class DoodadLine : public Serializable {
 public:
  DoodadLine();

  virtual nlohmann::json serialize() override;
  virtual void deserialize(const nlohmann::json &json) override;

  uint64_t &id() { return _id; }
  float &sx() { return _sx; }
  float &sy() { return _sy; }
  float &ex() { return _ex; }
  float &ey() { return _ey; }

 private:
  uint64_t _id;
  float _sx, _sy;
  float _ex, _ey;
};
