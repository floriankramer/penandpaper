#pragma once

#include <cstdint>

class IdGenerator {
 public:
  IdGenerator();

  uint64_t operator()();
  uint64_t next();

 private:
  uint64_t _next_id;
};
