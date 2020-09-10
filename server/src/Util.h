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
#pragma once

#include <functional>
#include <string>
#include <vector>

namespace util {
/**
 * @brief Applies f to every element in in and returns a new vector with the
 * results
 */
template <typename A, typename B>
std::vector<B> map(std::vector<A> in, std::function<B(const A &)> f) {
  std::vector<B> r;
  r.reserve(in.size());
  for (const A &a : in) {
    r.emplace_back(f(a));
  }
  return r;
}

inline std::vector<std::string> splitString(const std::string &s,
                                            char delimiter) {
  std::vector<std::string> parts;
  size_t begin = 0, end = 0;
  // Skip any leading whitespace
  while (begin < s.size() && s[begin] == delimiter) {
    begin++;
  }

  // split the string
  while (begin < s.size()) {
    // At this point s[begin] is not whitespace
    // Search for the next whitespace
    end = begin + 1;
    while (end < s.size() && s[end] != delimiter) {
      end++;
    }

    // s[begin:end] is now the longest whitespace free substring starting at
    // begin
    parts.emplace_back(s.substr(begin, end - begin));

    begin = end + 1;
    // skip any whitespace until the next none whitespace
    while (begin < s.size() && s[begin] == delimiter) {
      begin++;
    }
  }

  return parts;
}

std::vector<std::string> splitStringWs(const std::string &s);

/**
 * @return The string up to the num_words +1 occurence of a block of whitespace
 */
std::string firstWords(const std::string &s, size_t num_words,
                       bool with_trailing_ws = false);


std::vector<char> base16Encode(const void *data, size_t length);

std::vector<char> base64Encode(const void *data, size_t length);
std::string base64EncodeStr(const void *data, size_t length);

// encode three bytes in four characters
void base64Encode3(const void *src, char *dest);
std::vector<char> base64Decode(const char *data, size_t length);
// decode 4 characters to three byts
void base64Decode3(const char *src, void *dest);
}  // namespace util
