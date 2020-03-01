#include "IdGenerator.h"

IdGenerator::IdGenerator() : _next_id(0) {}

uint64_t IdGenerator::operator()() { return next(); }

uint64_t IdGenerator::next() { return _next_id++; }
