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
    _ids_search_index.add(c.col(0).text, c.col(0).text);
    c.next();
  }
}

void Wiki::onRequest(const httplib::Request &req, httplib::Response &resp) {
  std::vector<std::string> parts = util::splitString(req.path, '/');
  if (parts.size() != 3 &&
      !(parts.size() == 2 && (parts[1] == "list" || parts[1] == "complete"))) {
    LOG_ERROR << "Invalid wiki request at path " << req.path << LOG_END;
    resp.status = 400;
    resp.body = "Invalid wiki request.";
    return;
  }
  std::string action = parts[1];
  if (action == "list") {
    handleList(resp);
  } else if (action == "complete") {
    handleCompleteEntity(req, resp);
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

void Wiki::handleCompleteEntity(const httplib::Request &req,
                                httplib::Response &resp) {
  using nlohmann::json;
  struct Completion {
    QGramIndex::Match match;
    size_t num_words_used;
    std::string replaces;

    bool operator<(const Completion &other) { return match < other.match; }
  };
  try {
    json jreq = json::parse(req.body);
    std::string context = jreq.at("context");
    std::vector<std::string> parts = util::splitStringWs(context);
    std::vector<Completion> results;
    for (size_t i = 1; i <= parts.size(); ++i) {
      std::string word;
      // use the i last words
      for (size_t j = 0; j < i; ++j) {
        std::string next = parts[parts.size() - 1 - j];
        if (j > 0) {
          next += " ";
        }
        word = next + word;
      }
      std::vector<QGramIndex::Match> subres = _ids_search_index.query(word);
      if (subres.empty()) {
        // This avoid a string such as 'Midgard z' matching Midgard. As z
        // is not contained in Midgard the match is rather strange.
        // For multi word matches this still works. E.g
        // `Albert Ein` is going to match `Albert Einstein` as `Ein` matches
        // `Albert Einstein`
        break;
      }
      for (const QGramIndex::Match &m : subres) {
        // Ignore matches with a very low score.
        if (m.score > 0.3) {
          results.push_back({m, i, word});
        }
      }
    }
    std::sort(results.begin(), results.end(),
              [](const Completion &c1, const Completion &c2) {
                return c1.match.score > c2.match.score;
              });

    json j = std::vector<json>();
    for (size_t i = 0; i < results.size(); ++i) {
      json completion;
      // replace the last offset characters
      // TODO: Alternatively it might be easier to send back a replacement
      // for the entirety of the context, due to the way codemirror does
      // autocompletion
      completion["offset"] = results[i].match.value.size();
      // Extract the part of the context that this replacement doesn't use
      std::string prefix =
          util::firstWords(context, parts.size() - results[i].num_words_used);
      // append the replacement to the unused part of the context
      completion["value"] = prefix + " [" + results[i].match.value + "](" +
                            results[i].match.value + ")";
      completion["name"] = results[i].match.value;
      completion["replaces"] =
          results[i].replaces + " " + std::to_string(results[i].match.score);
      j.push_back(completion);
    }

    resp.status = 200;
    resp.set_header("Content-Type", "application/json");
    resp.body = j.dump();
  } catch (const std::exception &e) {
    LOG_WARN << "Wiki: Error while handling a completion request: " << e.what()
             << LOG_END;
    resp.status = 400;
    resp.body = "Malformed completion request.";
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
    _pages_table.update({{"value", req.body}},
                        DbCondition("id", DbCondition::Type::EQ, id) &&
                            DbCondition("attribute", DbCondition::Type::EQ,
                                        std::string("text")));
    // Invalidate the markdown cache
    auto mit = _markdown_cache.find(id);
    if (mit != _markdown_cache.end()) {
      _markdown_cache.erase(mit);
    }
  } else {
    _known_ids.insert(id);
    _ids_search_index.add(id, id);
    _pages_table.insert({id, std::string("text"), req.body});
  }
  resp.status = 200;
  resp.body = "Save succesfull";
}

void Wiki::handleDelete(const std::string &id, const httplib::Request &req,
                        httplib::Response &resp) {
  _pages_table.erase(DbCondition("id", DbCondition::Type::EQ, id));
  _known_ids.erase(id);
  _ids_search_index.remove(id);
  resp.status = 200;
  resp.body = "Save succesfull";
}
