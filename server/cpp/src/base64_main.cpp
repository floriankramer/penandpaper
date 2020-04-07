#include <cstdio>
#include <cstring>
#include <iostream>

#include "Util.h"

int main(int argc, char **argv) {
  using std::cin;
  using std::cout;

  if (argc != 2) {
    printf("Usage: %s -d/-e\n", argv[0]);
    return 1;
  }

  std::vector<char> in;
  char buffer[4096];
  while (!cin.eof()) {
    cin.read(buffer, 4096);
    size_t num_read = cin.gcount();
    in.insert(in.end(), buffer, buffer + num_read);
  }
  std::vector<char> result;
  if (strcmp(argv[1], "-d") == 0) {
    result = util::base64Decode(in.data(), in.size());
  } else {
    result = util::base64Encode(in.data(), in.size());
  }
  cout.write(result.data(), result.size());
}
