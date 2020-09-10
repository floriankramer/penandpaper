/**
 * Copyright 2020 Florian Kramer
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
#include "Random.h"

#ifndef WIN32
#include <sys/random.h>
#else
#include <Windows.h>
#endif

#include "Util.h"

std::vector<char> Random::secureRandomBytes(size_t length) {
  std::vector<char> buffer(length);
#ifndef WIN32
  getrandom(buffer.data(), buffer.size(), 0);
#else
  BCRYPT_ALG_HANDLE handle;
  BCryptOpenAlgorithmProvider(&handle, BCRYPT_RNG_ALGORITHM, NULL, 0);
  BCryptGenRandom(handle, reinterpret_cast<PUCHAR>(buffer.data()),
                  buffer.size(), 0);
  BCryptCloseAlgorithmProvider(handle, 0);
#endif
  return buffer;
}

std::string Random::secureRandomString(size_t length) {
  size_t num_bytes = length * 3;
  // divide and ceil
  num_bytes = (num_bytes + ((4 - (num_bytes % 4)) % 4)) / 4;
  std::vector<char> buffer(num_bytes);
#ifndef WIN32
  getrandom(buffer.data(), num_bytes, 0);
#else
  BCRYPT_ALG_HANDLE handle;
  BCryptOpenAlgorithmProvider(&handle, BCRYPT_RNG_ALGORITHM, NULL, 0);
  BCryptGenRandom(handle, reinterpret_cast<PUCHAR>(buffer.data()), num_bytes,
                  0);
  BCryptCloseAlgorithmProvider(handle, 0);
#endif
  std::vector<char> encoded = util::base64Encode(buffer.data(), buffer.size());
  return std::string(encoded.begin(), encoded.begin() + length);
}
