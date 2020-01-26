#pragma once

#include <string>

class HttpServer {
public:
  HttpServer(bool do_keycheck=true);

private:
  void run();
  std::string genKey();
  std::string guessMimeType(const std::string &path);

  bool _do_keycheck;
};
