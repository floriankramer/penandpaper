#pragma once

#include <vector>
#include <string>

class File {
public:
  static std::vector<std::string> listDir(const std::string &path);
};
