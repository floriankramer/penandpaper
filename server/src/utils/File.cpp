#include "File.h"

#include <cstring>
#include <type_traits>

#ifdef linux
#include <dirent.h>
#endif

std::vector<std::string> File::listDir(const std::string &path) {
  std::vector<std::string> entries;
#ifdef linux
  DIR *dir = opendir(path.c_str());
  struct dirent *ent;
  if (dir) {
    try {
      while ((ent = readdir(dir)) != nullptr) {
        if (strcmp(".", ent->d_name) != 0 && strcmp("..", ent->d_name) != 0) {
          entries.emplace_back(ent->d_name);
        }
      }
    } catch (...) {
      closedir(dir);
      throw;
    }
    closedir(dir);
  }
#else
  std::static_assert(false, "File::listDir is not supported on this platform.");
#endif
  return entries;
}
