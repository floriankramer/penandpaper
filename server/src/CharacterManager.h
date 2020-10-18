#pragma once

#include <unordered_map>

#include "HttpServer.h"
#include "LuaScript.h"

class CharacterManager : public HttpServer::RequestHandler {
 public:
  struct CharacterBlock {
    std::string heading;
    int width = 4;
    // TODO: attributes should probably be a vector so they have a fixed order.
    std::unordered_map<std::string, LuaScript::Variant> attributes;
    std::unordered_map<std::string, std::string> actions;
  };

  struct CharacterTemplate {
    std::vector<CharacterBlock> blocks;
  };

  CharacterManager();

  virtual HttpServer::HttpResponse onRequest(
      const HttpServer::HttpRequest &req);
};
