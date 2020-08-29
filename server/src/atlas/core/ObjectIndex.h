#pragma once

#include <string>

#include "Image.h"

namespace atlas {
class ObjectIndex {
 public:
  struct Object {
    Image image;
    std::string name;
  };

  ObjectIndex();
  virtual ~ObjectIndex();

  void load(const std::string &path);

  const std::vector<Object> &objects() const;

 private:
  std::vector<Object> _objects;
};

}  // namespace atlas
