#include "Wiki.h"

#include "Logger.h"
#include "Util.h"

Wiki::Wiki(Database *db)
    : _db(db),
      _pages_table(_db->createTable(
          "wiki", {{"id", DbDataType::TEXT}, {"content", DbDataType::TEXT}})) {
  DbCursor c = _pages_table.query();
  while (!c.done()) {
    _known_ids.insert(c.col(0).text);
    c.next();
  }
}

void Wiki::onRequest(const httplib::Request &req, httplib::Response &resp) {
  std::vector<std::string> parts = util::splitString(req.path, '/');
  if (parts.size() != 3) {
    LOG_ERROR << "Invalid wiki request at path " << req.path << LOG_END;
    resp.status = 400;
    resp.body = "Invalid wiki request.";
    return;
  }
  std::string action = parts[1];
  if (action == "get") {
  } else if (action == "save") {
  } else if (action == "delete") {
  } else {
    LOG_ERROR << "Unknown wiki action " << action << " at " << req.path
              << LOG_END;
    resp.status = 400;
    resp.body = "Invalid wiki request.";
    return;
  }
}

void Wiki::handleGet(const std::string &id, httplib::Response &resp) {
  if (_known_ids.find(id) != _known_ids.end()) {
    DbCursor c = _pages_table.query("`id` = '" + id + "'");
    resp.status = 200;
    resp.body = c.col(1).text;
  } else {
    resp.status = 404;
    resp.body = "No such wiki entry";
  }
}

void Wiki::handleSave(const std::string &id, const httplib::Request &req,
                      httplib::Response &resp) {
  // TODO
}

void Wiki::handleDelete(const std::string &id, const httplib::Request &req,
                        httplib::Response &resp) {
  // TODO
}
