#include "Wiki.h"

#include <cstdlib>
#include <nlohmann/json.hpp>

#include "Logger.h"
#include "Markdown.h"
#include "Util.h"

Wiki::Wiki(Database *db)
    : _db(db),
      _pages_table(_db->createTable("wiki", {{"id", DbDataType::TEXT},
                                             {"attribute", DbDataType::TEXT},
                                             {"value", DbDataType::TEXT}})) {
  DbCursor c = _pages_table.query();
  while (!c.done()) {
    _known_ids.insert(c.col(0).text);
    c.next();
  }
}

void Wiki::onRequest(const httplib::Request &req, httplib::Response &resp) {
  std::vector<std::string> parts = util::splitString(req.path, '/');
  if (parts.size() != 3 && !(parts.size() == 2 && parts[1] == "list")) {
    LOG_ERROR << "Invalid wiki request at path " << req.path << LOG_END;
    resp.status = 400;
    resp.body = "Invalid wiki request.";
    return;
  }
  std::string action = parts[1];
  if (action == "list") {
    handleList(resp);
  } else if (action == "get") {
    handleGet(parts[2], resp);
  } else if (action == "raw") {
    handleRaw(parts[2], resp);
  } else if (action == "save") {
    handleSave(parts[2], req, resp);
  } else if (action == "delete") {
    handleDelete(parts[2], req, resp);
  } else {
    LOG_ERROR << "Unknown wiki action " << action << " at " << req.path
              << LOG_END;
    resp.status = 400;
    resp.body = "Invalid wiki request.";
    return;
  }
}

void Wiki::handleList(httplib::Response &resp) {
  using nlohmann::json;
  json entries;
  for (const std::string &it : _known_ids) {
    entries.push_back(it);
  }
  resp.status = 200;
  resp.body = entries.dump();
}

void Wiki::handleGet(const std::string &id, httplib::Response &resp) {
  if (_known_ids.find(id) != _known_ids.end()) {
    if (_markdown_cache.find(id) != _markdown_cache.end()) {
      resp.status = 200;
      resp.body = _markdown_cache[id];
    } else {
      DbCursor c =
          _pages_table.query(DbCondition("id", DbCondition::Type::EQ, id));
      std::string raw = c.col(2).text;
      Markdown m(raw);
      try {
        std::string parsed = m.process();
        if (_markdown_cache.size() > MAX_MARKDOWN_CACHE_SIZE) {
          unsigned int seed = time(NULL);
          _markdown_cache.erase(_markdown_cache.begin());
        }
        _markdown_cache[id] = parsed;
        resp.status = 200;
        resp.body = parsed;
      } catch (const std::exception &e) {
        resp.status = 200;
        resp.body = "Unable to parse the input markdown<br/>" + raw;
      }
    }
  } else {
    resp.status = 404;
    resp.body = "No such wiki entry";
  }
}

void Wiki::handleRaw(const std::string &id, httplib::Response &resp) {
  if (_known_ids.find(id) != _known_ids.end()) {
    DbCursor c =
        _pages_table.query(DbCondition("id", DbCondition::Type::EQ, id));
    resp.status = 200;
    resp.body = c.col(2).text;
  } else {
    resp.status = 404;
    resp.body = "No such wiki entry";
  }
}

void Wiki::handleSave(const std::string &id, const httplib::Request &req,
                      httplib::Response &resp) {
  if (_known_ids.find(id) != _known_ids.end()) {
    // TODO: Create a merged DbCondition that only targets the text attribute
    _pages_table.update(
        {{"content", req.body}},
        DbCondition("id", DbCondition::Type::EQ, id) &&
            DbCondition("attr", DbCondition::Type::EQ, std::string("text")));
    // Invalidate the markdown cache
    auto mit = _markdown_cache.find(id);
    if (mit != _markdown_cache.end()) {
      _markdown_cache.erase(mit);
    }
  } else {
    _known_ids.insert(id);
    _pages_table.insert({id, std::string("text"), req.body});
  }
  resp.status = 200;
  resp.body = "Save succesfull";
}

void Wiki::handleDelete(const std::string &id, const httplib::Request &req,
                        httplib::Response &resp) {
  _pages_table.erase(DbCondition("id", DbCondition::Type::EQ, id));
  _known_ids.erase(id);
  resp.status = 200;
  resp.body = "Save succesfull";
}
