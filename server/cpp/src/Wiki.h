#pragma once

#include <unordered_set>
#include <unordered_map>

#include "Database.h"
#include "HttpServer.h"

class Wiki : public HttpServer::RequestHandler {
 public:
  Wiki(Database *db);

  virtual void onRequest(const httplib::Request &req, httplib::Response &resp);

 private:
  void handleList(httplib::Response &resp);
  void handleGet(const std::string &id, httplib::Response &resp);
  void handleRaw(const std::string &id, httplib::Response &resp);
  void handleSave(const std::string &id, const httplib::Request &req, httplib::Response &resp);
  void handleDelete(const std::string &id, const httplib::Request &req, httplib::Response &resp);

  Database *_db;
  Table _pages_table;
  std::unordered_set<std::string> _known_ids;
  std::unordered_map<std::string, std::string> _markdown_cache;

  static constexpr size_t MAX_MARKDOWN_CACHE_SIZE = 16;
};
