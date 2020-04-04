#pragma once

#include <unordered_set>

#include "Database.h"
#include "HttpServer.h"

class Wiki : public HttpServer::RequestHandler {
 public:
  Wiki(Database *db);

  virtual void onRequest(const httplib::Request &req, httplib::Response &resp);

 private:
  void handleGet(const std::string &id, httplib::Response &resp);
  void handleSave(const std::string &id, const httplib::Request &req, httplib::Response &resp);
  void handleDelete(const std::string &id, const httplib::Request &req, httplib::Response &resp);

  Database *_db;
  Table _pages_table;
  std::unordered_set<std::string> _known_ids;
};
