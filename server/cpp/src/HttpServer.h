#pragma once

#include <string>

class HttpServer {
public:
  HttpServer();

private:
  void run();
  std::string genKey();
  std::string guessMimeType(const std::string &path);
};
