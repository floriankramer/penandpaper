#pragma once

#include <nlohmann/json.hpp>

class Serializable {
public:
  virtual nlohmann::json serialize() = 0;
  virtual void deserialize(const nlohmann::json &json) = 0;
};
