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

inline std::vector<std::string> splitString(const std::string &s, char delimiter) {
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

std::vector<char> base64Encode(const void *data, size_t length);
std::vector<char> base64Decode(const char *data, size_t length);
}  // namespace util
