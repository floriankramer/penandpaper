#include "Random.h"
#include <sys/random.h>

#include "Util.h"


std::string Random::secureRandomString(size_t length) {
  size_t num_bytes = length * 3;
  // divide and ceil
  num_bytes = (num_bytes + ((4 - (num_bytes % 4)) % 4)) / 4;
  std::vector<char> buffer(num_bytes);
  getrandom(buffer.data(), num_bytes, 0);
  std::vector<char> encoded = util::base64Encode(buffer.data(), buffer.size());
  return std::string(encoded.begin(), encoded.begin() + length);
}
