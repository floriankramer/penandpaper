#pragma once

#include <functional>
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
}  // namespace util
