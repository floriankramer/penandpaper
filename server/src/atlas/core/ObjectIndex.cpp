#include "ObjectIndex.h"

#include "../Logger.h"
#include "../utils/File.h"

namespace atlas {

ObjectIndex::ObjectIndex() {}
ObjectIndex::~ObjectIndex() {}

void ObjectIndex::load(const std::string &path) {
  std::vector<std::string> files = File::listDir(path);
  for (const std::string &file : files) {
    if (file.size() < 4 || file.substr(file.size() - 4) != ".png") {
      continue;
    }
    _objects.emplace_back();
    _objects.back().name = file.substr(0, file.size() - 4);
    _objects.back().image.load(path + "/" + file);
  }
  LOG_INFO << "Loaded " << _objects.size() << " objects." << LOG_END;
}

const std::vector<ObjectIndex::Object> &ObjectIndex::objects() const {
  return _objects;
}

}  // namespace atlas
