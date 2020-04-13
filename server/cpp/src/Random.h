#pragma once

#include <string>

class Random {
public:
  /**
   * @brief Generate a random string using urand. Will block until urand is
   *        initialized. The string will be length characters long, with every
   *        four characters representing three bytes of random data.
   */
  static std::string secureRandomString(size_t length);
};
