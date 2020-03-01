#pragma once

#include "Permissions.h"
#include <string>

struct Player {
  size_t id;
  std::string uid;
  Permissions permissions;
  std::string name;
};
