/**
 * Copyright 2020 Florian Kramer
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
#include "Wiki.h"

#include <cstdlib>
#include <nlohmann/json.hpp>

#include "Logger.h"
#include "Markdown.h"
#include "Util.h"

const std::string Wiki::IDX_COL = "numid";
const std::string Wiki::ID_COL = "id";
const std::string Wiki::PREDICATE_COL = "predicate";
const std::string Wiki::VALUE_COL = "value";
const std::string Wiki::FLAG_COL = "flags";
const std::string Wiki::TEXT_ATTR = "text";

const int Wiki::ATTR_INTERESTING = 1;
const int Wiki::ATTR_INHERITABLE = 2;
const int Wiki::ATTR_DATE = 4;

Wiki::Wiki(Database *db)
    : _db(db),
      _pages_table(
          _db->createTable("wiki", {{IDX_COL, DbDataType::AUTO_INCREMENT},
                                    {ID_COL, DbDataType::TEXT},
                                    {PREDICATE_COL, DbDataType::TEXT},
                                    {VALUE_COL, DbDataType::TEXT},
                                    {FLAG_COL, DbDataType::INTEGER}})),
      _root(&_pages_table) {
  _lookup_attributed_bound =
      std::bind(&Wiki::lookupAttribute, this, std::placeholders::_1,
                std::placeholders::_2);

  // Build the entry tree
  DbCursor c = _pages_table.query();
  // initially create a list of entries
  while (!c.done()) {
    std::string id = c.col(1).text;
    if (_entry_map.count(id) == 0) {
      _entry_map.insert(
          std::make_pair(id, new Entry(id, &_root, &_pages_table)));
    }
    c.next();
  }
  // Then build the tree and assign all attributes
  c.reset();
  std::vector<int64_t> duplicates_to_erase;
  std::vector<Entry *> entries_with_invalid_parents;
  while (!c.done()) {
    int64_t idx = c.col(0).integer;
    std::string id = c.col(1).text;
    std::string predicate = c.col(2).text;
    std::string value = c.col(3).text;
    int64_t flags = c.col(4).integer;
    auto it = _entry_map.find(id);
    if (it != _entry_map.end()) {
      if (predicate == "parent") {
        if (value != "root") {
          auto pit = _entry_map.find(value);
          if (pit != _entry_map.end()) {
            it->second->reparent(pit->second);
          } else {
            LOG_WARN << "The entry " << id << " refers to a nonexistant parent."
                     << value << LOG_END;
            entries_with_invalid_parents.push_back(it->second);
          }
        }
      }
      IndexedAttributeData d;
      d.idx = idx;
      d.data.value = value;
      d.data.flags = flags;

      MdNode md = tryProcessMarkdown(d.data.value);
      std::ostringstream html;
      md.toHTML(html, _lookup_attributed_bound);
      d.data.value_markdown_html = html.str();

      if (!it->second->loadAttribute(predicate, d)) {
        duplicates_to_erase.push_back(idx);
      }
    } else {
      LOG_ERROR << "Wiki table modified during wiki loading." << LOG_END;
    }
    c.next();
  }
  // Ensure entries with invalid parents are properly children of root
  for (Entry *e : entries_with_invalid_parents) {
    AttributeData d;
    d.flags = 0;
    d.value = "root";
    auto v = e->getAttribute("parent");
    if (v != nullptr && v->size() != 0) {
      e->setAttribute("parent", &(*v)[0], d);
    } else {
      e->addAttribute("root", d);
    }
  }

  // Compute the attribute inheritance
  _root.updateInheritedAttributes();

  // Delete duplicate entries in the database. According to the spec they can't
  // exist due to the definition of attribute identity.
  for (int64_t idx : duplicates_to_erase) {
    LOG_INFO << "Removign a duplicate attribute with idx " << idx << LOG_END;
    _pages_table.erase(DbCondition(IDX_COL, DBCT::EQ, idx));
  }

  // Reparent all parentless nodes to the root. This will also
  // ensure that every node will be deleted once this wiki instance
  // is destructed.
  // Also build the search index
  for (auto &p : _entry_map) {
    if (p.second->parent() == nullptr) {
      p.second->reparent(&_root);
    }
    addToSearchIndex(p.second);
    addToDateIndex(p.second);
  }
}

void Wiki::onRequest(const httplib::Request &req, httplib::Response &resp) {
  std::vector<std::string> parts = util::splitString(req.path, '/');
  LOG_INFO << "Wiki " << req.method << " request for " << req.path << LOG_END;
  if (parts.size() < 2) {
    LOG_ERROR << "Invalid wiki request at path " << req.path << LOG_END;
    resp.status = 400;
    resp.body = "Invalid wiki request.";
    return;
  }
  std::string action = parts[1];
  if (action == "list" && parts.size() == 2) {
    handleList(resp);
    return;
  }
  if (action == "complete" && parts.size() == 3) {
    if (parts[2] == "entry") {
      handleCompleteEntity(req, resp);
      return;
    } else if (parts[2] == "reference") {
      handleCompleteAttrRef(req, resp);
      return;
    }
  }

  if (action == "autolink" && parts.size() > 1 && parts.size() < 4) {
    if (parts.size() == 3) {
      handleAutolink(parts[2], req, resp);
    } else {
      handleAutolinkAll(req, resp);
    }
    return;
  }

  if (action == "timeline" && parts.size() == 2) {
    handleTimeline(req, resp);
    return;
  }
  if (action == "quicksearch" && parts.size() == 2 && req.method == "POST") {
    handleQuicksearch(req, resp);
    return;
  }

  if (parts.size() != 3) {
    LOG_ERROR << "Invalid wiki request at path " << req.path << LOG_END;
    resp.status = 400;
    resp.body = "Invalid wiki request.";
    return;
  }
  if (action == "get") {
    handleGet(parts[2], resp);
  } else if (action == "raw") {
    handleRaw(parts[2], resp);
  } else if (action == "save") {
    handleSave(parts[2], req, resp);
  } else if (action == "delete") {
    handleDelete(parts[2], req, resp);
  } else if (action == "context") {
    handleContext(parts[2], resp);
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
    for (size_t num_words_used = 1; num_words_used <= parts.size();
         ++num_words_used) {
      std::string word;
      // use the num_words_used last words
      for (size_t j = 0; j < num_words_used; ++j) {
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
          results.push_back(
              {.match = m, .num_words_used = num_words_used, .replaces = word});
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
      std::string id = results[i].match.value.value;
      std::string name = results[i].match.value.alias;
      // Extract the part of the context that this replacement doesn't use
      std::string prefix = util::firstWords(
          context, parts.size() - results[i].num_words_used, true);
      // append the replacement to the unused part of the context
      completion["value"] = prefix + "[" + name + "](" + id + ")";
      completion["name"] = name;
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

void Wiki::handleCompleteAttrRef(const httplib::Request &req,
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
    std::vector<QGramIndex::Match> results =
        _attr_ref_search_index.query(context);

    json j = std::vector<json>();
    for (size_t i = 0; i < results.size(); ++i) {
      json completion;
      const QGramIndex::Match &m = results[i];
      // append the replacement to the unused part of the context
      completion["value"] = m.value.value;
      completion["name"] = m.value.alias;
      completion["replaces"] = context;
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

  struct DfsLevel {
    Entry *entry;
    size_t child_index;
    json j;
  };

  // do a dfs on the entry tree
  std::vector<DfsLevel> dfs_stack;
  dfs_stack.push_back({&_root, 0, json()});
  dfs_stack.back().j["name"] = "root";
  dfs_stack.back().j["id"] = nullptr;
  dfs_stack.back().j["children"] = json::array();

  while (!dfs_stack.empty()) {
    DfsLevel &l = dfs_stack.back();
    if (l.child_index >= l.entry->children().size()) {
      // sort the nodes children alphabetically
      if (l.j.count("children") > 0) {
        std::sort(l.j["children"].begin(), l.j["children"].end(),
                  [](const json &left, const json &right) {
                    return left["name"].get<std::string>() <
                           right["name"].get<std::string>();
                  });
      }
      // we are done with this node
      if (dfs_stack.size() == 1) {
        // we are done with the root node
        break;
      }
      DfsLevel &parent = dfs_stack[dfs_stack.size() - 2];
      parent.j["children"].push_back(l.j);
      dfs_stack.pop_back();
    } else {
      // the node has more children
      size_t ci = l.child_index;
      Entry *child = l.entry->children()[ci];
      l.child_index++;
      // this invalidates the reference l
      json j;
      j["name"] = child->name();
      j["id"] = child->id();
      j["children"] = json::array();
      dfs_stack.push_back({child, 0, j});
    }
  }

  resp.status = 200;
  resp.body = dfs_stack.back().j.dump();
}

void Wiki::handleGet(const std::string &id, httplib::Response &resp) {
  using nlohmann::json;
  auto it = _entry_map.find(id);
  if (it != _entry_map.end()) {
    json direct;
    for (auto &ait : it->second->attributes()) {
      for (const IndexedAttributeData &a : ait.second) {
        json attr = a.toJson();
        attr["predicate"] = ait.first;
        direct.push_back(attr);
      }
    }

    json inherited = json::array();
    for (auto &ait : it->second->inherited_attributes()) {
      for (const IndexedAttributeData &a : ait.second) {
        json attr = a.toJson();
        attr["predicate"] = ait.first;
        inherited.push_back(attr);
      }
    }
    json j;
    j["name"] = it->second->name();
    j["direct"] = direct;
    j["inherited"] = inherited;
    resp.status = 200;
    resp.body = j.dump();
  } else {
    resp.status = 404;
    resp.body = "No such wiki entry";
  }
}

void Wiki::handleRaw(const std::string &id, httplib::Response &resp) {
  using nlohmann::json;
  auto it = _entry_map.find(id);
  if (it != _entry_map.end()) {
    json direct;
    for (auto &ait : it->second->attributes()) {
      for (const IndexedAttributeData &a : ait.second) {
        json attr = a.toJson();
        attr["predicate"] = ait.first;
        attr["value"] = a.data.value;
        direct.push_back(attr);
      }
    }
    json inherited = json::array();
    for (auto &ait : it->second->inherited_attributes()) {
      for (const IndexedAttributeData &a : ait.second) {
        json attr = a.toJson();
        attr["predicate"] = ait.first;
        inherited.push_back(attr);
      }
    }
    json j;
    j["name"] = it->second->name();
    j["direct"] = direct;
    j["inherited"] = inherited;
    resp.status = 200;
    resp.body = j.dump();
  } else {
    resp.status = 404;
    resp.body = "No such wiki entry";
  }
}

void Wiki::handleSave(const std::string &id, const httplib::Request &req,
                      httplib::Response &resp) {
  using nlohmann::json;
  if (id == "root") {
    resp.status = 400;
    resp.body = "`root` is not an allowed id.";
    return;
  }
  try {
    json jreq = json::parse(req.body);
    std::vector<Attribute> attributes;
    std::string new_parent_id = "root";
    for (const json &attr : jreq) {
      Attribute a;
      a.predicate = attr.at("predicate").get<std::string>();
      a.data.flags = 0;
      a.data.flags |=
          attr.at("isInteresting").get<bool>() ? ATTR_INTERESTING : 0;
      a.data.flags |=
          attr.at("isInheritable").get<bool>() ? ATTR_INHERITABLE : 0;
      a.data.flags |= attr.at("isDate").get<bool>() ? ATTR_DATE : 0;
      a.data.value = attr.at("value").get<std::string>();

      MdNode md = tryProcessMarkdown(a.data.value);
      std::ostringstream html;
      md.toHTML(html, _lookup_attributed_bound);
      a.data.value_markdown_html = html.str();

      if (a.predicate == "parent") {
        new_parent_id = a.data.value;
      }
      attributes.push_back(a);
    }
    Entry *parent = &_root;
    if (new_parent_id != "root") {
      auto pit = _entry_map.find(new_parent_id);
      if (pit != _entry_map.end()) {
        parent = pit->second;
      } else {
        LOG_WARN << "Entry references unknown parent " << new_parent_id
                 << LOG_END;
      }
    }

    auto it = _entry_map.find(id);
    if (it != _entry_map.end()) {
      removeFromSearchIndex(it->second);
      removeFromDateIndex(it->second);
      it->second->setAttributes(attributes);
      if (parent != it->second->parent()) {
        it->second->reparent(parent);
      }
      addToSearchIndex(it->second);
      addToDateIndex(it->second);
    } else {
      Entry *e = parent->addChild(id);
      e->setAttributes(attributes);
      _entry_map[id] = e;
      addToSearchIndex(e);
      addToDateIndex(e);
    }
    resp.status = 200;
    resp.body = "Save succesfull";
    return;
  } catch (const std::exception &e) {
    LOG_WARN << "Unable to process a save request: " << e.what() << LOG_END;
  }
  resp.status = 400;
  resp.body = "Invalid request";
}

void Wiki::handleDelete(const std::string &id, const httplib::Request &req,
                        httplib::Response &resp) {
  auto it = _entry_map.find(id);
  if (it == _entry_map.end()) {
    resp.status = 400;
    resp.body = "Unable to delete the entry.";
    return;
  }
  // run a bfs on the nodes subtree to generate an inverse topological
  // sorting.
  std::vector<Entry *> sorting;
  std::vector<Entry *> to_process;
  to_process.push_back(it->second);
  sorting.push_back(it->second);
  // This works for a tree, but might not work for all graphs.
  while (!to_process.empty()) {
    Entry *e = to_process.back();
    to_process.pop_back();
    for (Entry *c : e->children()) {
      sorting.push_back(c);
      to_process.push_back(c);
    }
  }
  // process the topological sorting backwards to ensure children are
  // processed before their parents
  for (size_t i = sorting.size(); i > 0; i--) {
    Entry *e = sorting[i - 1];
    _entry_map.erase(e->id());
    _pages_table.erase(DbCondition(ID_COL, DBCT::EQ, e->id()));

    // Erase all mentions from the search index
    removeFromSearchIndex(e);
    removeFromDateIndex(e);
  }

  // This will recursively free the memory of the children
  delete it->second;
  resp.status = 200;
  resp.body = "Deletion succesfull";
  return;
}

void Wiki::handleContext(const std::string &id, httplib::Response &resp) {
  using nlohmann::json;
  auto it = _entry_map.find(id);
  if (it == _entry_map.end()) {
    resp.body = "No such entry";
    resp.status = 400;
    return;
  }
  std::unordered_set<std::string> referenced_ids;
  auto entryToContextJson = [](const Entry *e) {
    json ej;
    ej["id"] = e->id();
    ej["name"] = e->name();
    json eja = json::array();
    for (const auto &ait : e->attributes()) {
      for (const IndexedAttributeData &a : ait.second) {
        if (a.data.flags & ATTR_INTERESTING) {
          json attr = a.toJson();
          attr["predicate"] = ait.first;
          eja.push_back(attr);
        }
      }
    }
    ej["attributes"] = eja;
    return ej;
  };

  json r = json::array();
  // go through the entries parent and agglomorate interesting attributes
  Entry *e = it->second->parent();
  while (e != nullptr) {
    if (referenced_ids.count(e->id()) == 0) {
      json ej = entryToContextJson(e);
      if (ej.at("attributes").size() > 0) {
        r.push_back(ej);
      }
      referenced_ids.insert(e->id());
    }
    e = e->parent();
  }
  // go through all linked entries
  MdNode md = tryProcessMarkdown(getText(it->second));
  md.traverse(
      [this, &r, &referenced_ids, &entryToContextJson](const MdNode &e) {
        if (e.type() == MdNodeType::LINK) {
          const LinkMdNode *link = static_cast<const LinkMdNode *>(&e);
          auto it = _entry_map.find(link->target());
          if (it != _entry_map.end()) {
            if (referenced_ids.count(it->second->id()) == 0) {
              json ej = entryToContextJson(it->second);
              if (ej.at("attributes").size() > 0) {
                r.push_back(ej);
              }
              referenced_ids.insert(it->second->id());
            }
          }
        }
      });
  resp.body = r.dump();
  resp.status = 200;
}

void Wiki::handleTimeline(const httplib::Request &req,
                          httplib::Response &resp) {
  using nlohmann::json;
  json j;

  json events = json::array();
  if (!_dates.empty()) {
    Date last_date = _dates.begin()->first;
    for (const auto &date : _dates) {
      for (const auto &event : date.second) {
        json event_j;
        event_j["date"] = event.date;
        event_j["name"] = event.entry->name();
        event_j["id"] = event.entry->id();
        event_j["predicate"] = event.predicate;
        event_j["firstDifferentField"] =
            date.first.firstDifferentField(last_date);
        last_date = date.first;
        events.push_back(event_j);
      }
    }
  }
  j["events"] = events;
  resp.body = j.dump();
  resp.status = 200;
}

void Wiki::handleQuicksearch(const httplib::Request &req,
                             httplib::Response &resp) {
  using nlohmann::json;
  json j = json::array();
  std::unordered_set<std::string> returned_ids;

  std::string query = req.body.substr(0, 512);
  std::vector<QGramIndex::Match> matches = _ids_search_index.query(query);
  size_t num = std::min(size_t(64), matches.size());
  for (size_t i = 0; i < num; ++i) {
    if (returned_ids.count(matches[i].value.value) == 0) {
      returned_ids.insert(matches[i].value.value);
      json res;
      res["name"] = matches[i].value.alias;
      res["id"] = matches[i].value.value;
      j.push_back(res);
    }
  }
  resp.body = j.dump();
  resp.status = 200;
}

void Wiki::handleAutolink(const std::string &id, const httplib::Request &req,
                          httplib::Response &resp) {
  auto it = _entry_map.find(id);
  if (it == _entry_map.end()) {
    resp.body = "No such entry";
    resp.status = 400;
    return;
  }
  autoLink(it->second, 0.9);
  resp.body = "Ok";
  resp.status = 200;
}

void Wiki::handleAutolinkAll(const httplib::Request &req,
                             httplib::Response &resp) {
  for (auto it : _entry_map) {
    autoLink(it.second, 0.9);
  }
  resp.body = "Ok";
  resp.status = 200;
}

void Wiki::autoLink(Entry *e, double score_threshold) {
  const auto *texts = e->getAttribute(TEXT_ATTR);
  if (texts == nullptr || texts->empty()) {
    return;
  }
  for (const IndexedAttributeData &data : *texts) {
    const std::string &text = data.data.value;
    if (text.empty()) {
      continue;
    }

    std::ostringstream result;
    // a vector of alternating words and whitespace
    std::vector<std::string> ctx_entries;
    // keep between 16 and 32 characters worth of data in ctx_entries.
    // then whenever a full word was read apply the autocompletion. Apply any
    // results with score at least score_threshold. if nothing was applied throw
    // out the last two entries in the vector and write them to result. Ignore
    // any data inside of () or [] and discard the current context if such a
    // block is found.
    size_t pos = 0;
    size_t bracket_depth = 0;
    size_t sq_bracket_depth = 0;
    bool insideWs = std::isspace(text[0]);
    size_t start = 0;
    while (pos < text.size()) {
      char c = text[pos];
      // Start anew from the next pos
      bool clearContext = false;
      // We are on the character behind a word boundary
      bool addWord = false;

      // Ignore everything inside of [] or (). This prevents links being
      // matched again. It () may be used inside of normal text, so this is
      // slightly to strong, but for now will have to suffice.
      if (c == '[') {
        addWord |= sq_bracket_depth == 0;
        sq_bracket_depth++;
      } else if (c == '(') {
        addWord |= bracket_depth == 0;
        bracket_depth++;
      } else if (c == ']' && sq_bracket_depth > 0) {
        sq_bracket_depth--;
        clearContext |= sq_bracket_depth == 0;
      } else if (c == ')' && bracket_depth > 0) {
        bracket_depth--;
        clearContext |= bracket_depth == 0;
      }

      if (bracket_depth == 0 && sq_bracket_depth == 0 && !clearContext) {
        if (std::isspace(c) && !insideWs) {
          insideWs = true;
          addWord = true;
        } else if (!std::isspace(c) && insideWs) {
          insideWs = false;
          addWord = true;
        }
      }
      if (!addWord && !clearContext && pos + 1 >= text.size()) {
        addWord = true;
        // This is required to trigger autocompletion if the text ends in non
        // whitespace
        insideWs = !insideWs;
        // We need to include the last character
        pos++;
      }

      // We are at a word boundary, add the current word and do autocompletion.
      if (addWord) {
        ctx_entries.push_back(text.substr(start, pos - start));
        start = pos;
        if (ctx_entries.size() > 8) {
          result << ctx_entries[0];
          ctx_entries.erase(ctx_entries.begin());
        }
        // Only do autocompletion if we just left a word, otherwise we just read
        // whitespace which we don't complete.
        if (insideWs) {
          // Autocomplete on the current context
          size_t ctx_begin = 0;
          if (std::isspace(ctx_entries[0][0])) {
            ctx_begin++;
          }
          size_t ctx_end = ctx_entries.size();
          if (std::isspace(ctx_entries[ctx_end - 1][0])) {
            ctx_end--;
          }

          QGramIndex::Match best_match;
          best_match.score = 0;
          size_t max_score_words = 0;
          // Consider any number of 1 to (ctx_end - ctx_begin) words, always
          // starting from ctx_end - 1 up to ctx_begin. This allows for finding
          // multi word matches
          for (size_t i = ctx_end; i > ctx_begin; i -= 2) {
            std::string ctx = "";
            for (size_t j = ctx_end; j > i - 1; j--) {
              if ((j - 1 - ctx_begin) % 2 == 1) {
                // Replace all whitespace with a single space
                ctx = ' ' + ctx;
              } else {
                ctx = ctx_entries[j - 1] + ctx;
              }
            }
            std::vector<QGramIndex::Match> matches =
                _ids_search_index.query(ctx);
            if (matches.empty()) {
              break;
            }
            // We are only interested in the best match. Also don't link an
            // entry to itself.
            if (matches[0].score > score_threshold &&
                matches[0].score > best_match.score &&
                matches[0].value.value != e->id()) {
              best_match = matches[0];
              max_score_words = ctx_end - i + 1;
            }
          }
          if (best_match.score > score_threshold) {
            // we found a replacement
            size_t num_not_used = ctx_end - max_score_words;
            for (size_t i = 0; i < num_not_used; ++i) {
              result << ctx_entries[i];
            }

            // write the link
            result << "[" << best_match.value.alias << "]("
                   << best_match.value.value << ")";

            // erase the used elements
            ctx_entries.erase(ctx_entries.begin(),
                              ctx_entries.begin() + ctx_end);
          }
        }
      }

      if (clearContext) {
        // Add the current word
        ctx_entries.push_back(text.substr(start, pos + 1 - start));
        start = pos + 1;
        for (const std::string s : ctx_entries) {
          result << s;
        }
        ctx_entries.clear();
      }
      pos++;
    }

    for (std::string &s : ctx_entries) {
      // Write the remaining entries
      result << s;
    }

    AttributeData changed = data.data;
    changed.value = result.str();
    MdNode md = tryProcessMarkdown(changed.value);
    std::ostringstream html;
    md.toHTML(html, _lookup_attributed_bound);
    changed.value_markdown_html = html.str();

    // Update the cache, write to disk
    e->setAttribute(TEXT_ATTR, &data, changed);
  }
}

std::string Wiki::lookupAttribute(const std::string &id,
                                  const std::string &predicate) {
  auto eit = _entry_map.find(id);
  if (eit == _entry_map.end()) {
    return "[Id " + id + " unknown]";
  }
  const auto *vals = eit->second->getAttribute(predicate);
  if (vals == nullptr) {
    return "[" + predicate + " is empty]";
  }
  if (vals->size() == 1) {
    return (*vals)[0].data.value_markdown_html;
  } else {
    std::ostringstream out;
    for (size_t i = 0; i < vals->size(); ++i) {
      out << vals->at(i).data.value_markdown_html;
      if (i + 1 < vals->size()) {
        out << ", ";
      }
    }
    return out.str();
  }
}

void Wiki::removeFromSearchIndex(Entry *e) {
  // Remove it from the entry search index
  const auto *names = e->getAttribute("name");
  if (names != nullptr) {
    for (const IndexedAttributeData &name : *names) {
      _ids_search_index.remove(name.data.value, e->id());
    }
  }
  const auto *aliases = e->getAttribute("alias");
  if (aliases != nullptr) {
    for (const IndexedAttributeData &alias : *aliases) {
      _ids_search_index.remove(alias.data.value, e->id());
    }
  }
  _ids_search_index.remove(e->id(), e->id());

  // Remove it from the attr ref search index
  for (const auto &a : e->attributes()) {
    std::string s = e->id() + ":" + a.first;
    _attr_ref_search_index.remove(s, s);
  }
}

void Wiki::addToSearchIndex(Entry *e) {
  // Add to the entry search index
  const auto *names = e->getAttribute("name");
  if (names != nullptr) {
    for (const IndexedAttributeData &name : *names) {
      _ids_search_index.add(name.data.value, e->id());
    }
  }
  const auto *aliases = e->getAttribute("alias");
  if (aliases != nullptr) {
    for (const IndexedAttributeData &alias : *aliases) {
      _ids_search_index.add(alias.data.value, e->id());
    }
  }
  _ids_search_index.add(e->id(), e->id());

  // Add it to the attribute ref search index
  for (const auto &a : e->attributes()) {
    std::string s = e->id() + ":" + a.first;
    _attr_ref_search_index.add(s, s);
  }
}

void Wiki::addToDateIndex(Entry *e) {
  for (auto ait : e->attributes()) {
    for (const IndexedAttributeData &d : ait.second) {
      if (d.data.flags & ATTR_DATE) {
        try {
          Date date(d.data.value);
          EventData ed = {
              .date = d.data.value, .predicate = ait.first, .entry = e};
          _dates[date].push_back(ed);
        } catch (const std::exception &e) {
          LOG_WARN << "Failed to parse date for attr " << ait.first << " "
                   << d.data.value << " " << e.what() << LOG_END;
        }
      }
    }
  }
}

void Wiki::removeFromDateIndex(Entry *e) {
  for (auto ait : e->attributes()) {
    for (const IndexedAttributeData &d : ait.second) {
      if (d.data.flags & ATTR_DATE) {
        try {
          Date date(d.data.value);
          auto it = _dates.find(date);
          if (it != _dates.end()) {
            auto &v = it->second;
            EventData ed = {
                .date = d.data.value, .predicate = ait.first, .entry = e};
            v.erase(std::remove(v.begin(), v.end(), ed), v.end());
            if (v.empty()) {
              _dates.erase(it);
            }
          }
        } catch (const std::exception &e) {
          LOG_WARN << "Failed to parse date for attr " << ait.first << " "
                   << d.data.value << " " << e.what() << LOG_END;
        }
      }
    }
  }
}

MdNode Wiki::tryProcessMarkdown(const std::string &s) {
  // Process the attributes value as markdown
  Markdown m(s, _lookup_attributed_bound);
  try {
    return m.process();
  } catch (const std::exception &e) {
    LOG_WARN << "Error while processing attribute markdown: " << e.what()
             << LOG_END;
    return TextMdNode(s);
  }
}

std::string Wiki::getText(Entry *e) const {
  auto *v = e->getAttribute(TEXT_ATTR);
  if (v != nullptr && !v->empty()) {
    return (*v)[0].data.value;
  }
  return std::string();
}

// =============================================================================
// Entry
// =============================================================================

Wiki::Entry::Entry(Table *storage)
    : _id("root"), _parent(nullptr), _storage(storage) {}

Wiki::Entry::Entry(const std::string &id, Entry *parent, Table *storage)
    : _id(id), _parent(parent), _storage(storage) {
  if (_parent != nullptr) {
    _parent->_children.push_back(this);
    updateInheritedAttributes();
  }
}

Wiki::Entry::~Entry() {
  while (_children.size() > 0) {
    delete _children[0];
  }
  reparent(nullptr);
}

Wiki::Entry *Wiki::Entry::addChild(std::string child_id) {
  return new Entry(child_id, this, _storage);
}

void Wiki::Entry::reparent(Entry *new_parent) {
  if (_parent != nullptr) {
    _parent->_children.erase(
        std::remove(_parent->_children.begin(), _parent->_children.end(), this),
        _parent->_children.end());
  }
  _parent = new_parent;
  if (_parent != nullptr) {
    _parent->_children.push_back(this);
  }
  updateInheritedAttributes();
}

Wiki::Entry *Wiki::Entry::parent() { return _parent; }

const std::string &Wiki::Entry::name() const {
  if (hasAttribute("name")) {
    return (*getAttribute("name"))[0].data.value;
  }
  return _id;
}

const std::string &Wiki::Entry::id() const { return _id; }

const std::vector<Wiki::Entry *> &Wiki::Entry::children() const {
  return _children;
}

const std::unordered_map<std::string, std::vector<Wiki::IndexedAttributeData>>
    &Wiki::Entry::attributes() const {
  return _attributes;
}

const std::vector<Wiki::IndexedAttributeData> *Wiki::Entry::getAttribute(
    const std::string &predicate) const {
  auto it = _attributes.find(predicate);
  if (it != _attributes.end()) {
    return &(it->second);
  }
  return nullptr;
}

bool Wiki::Entry::loadAttribute(const std::string &predicate,
                                const IndexedAttributeData &value) {
  auto it = _attributes.find(predicate);
  if (it == _attributes.end()) {
    _attributes.insert(
        std::pair<std::string, std::vector<IndexedAttributeData>>(predicate,
                                                                  {value}));
  } else {
    // This only compares the value, none of the other properties
    if (std::find(it->second.begin(), it->second.end(), value) !=
        it->second.end()) {
      // The attribute already exists. This is not necessarily an error
      LOG_WARN << "Duplicate attribute " << _id << " - " << predicate << " - "
               << value.data.value << " while loading." << LOG_END;
      return false;
    } else {
      it->second.push_back(value);
    }
  }
  return true;
}

void Wiki::Entry::addAttribute(const std::string &predicate,
                               const AttributeData &value) {
  auto it = _attributes.find(predicate);
  IndexedAttributeData d;
  d.data = value;
  if (it == _attributes.end()) {
    int64_t idx = writeAttribute(predicate, value);
    d.idx = idx;
    _attributes[predicate].push_back(d);
  } else {
    if (std::find(it->second.begin(), it->second.end(), d) !=
        it->second.end()) {
      // The attribute already exists. This is not necessarily an error
      return;
    } else {
      int64_t idx = writeAttribute(predicate, value);
      d.idx = idx;
      it->second.push_back(d);
      // Write the attribute to the persistent storage.
    }
  }
  updateInheritedAttributes();
}

void Wiki::Entry::setAttributes(const std::vector<Attribute> &attributes) {
  // For now simply rewrite all our existing entries with new ones and delete
  // any that are to many.
  size_t num_attributes = 0;
  for (auto it : _attributes) {
    num_attributes += it.second.size();
  }
  size_t to_overwrite = std::min(num_attributes, attributes.size());
  LOG_DEBUG << "Will overwrite " << to_overwrite << " of the current "
            << num_attributes << " to store the new " << attributes.size()
            << " attributes " << LOG_END;

  std::unordered_map<std::string, std::vector<IndexedAttributeData>>
      new_attributes;

  // out position in the attributes vector
  size_t new_pos = 0;
  for (auto it : _attributes) {
    for (auto vit = it.second.begin(); vit != it.second.end(); ++vit) {
      if (new_pos < attributes.size()) {
        // update
        const Attribute &a = attributes[new_pos];
        LOG_DEBUG << "Overwriting attribute " << it.first << "(" << vit->idx
                  << ") with " << a.predicate << LOG_END;
        // update the database
        _storage->update({{PREDICATE_COL, a.predicate},
                          {VALUE_COL, a.data.value},
                          {FLAG_COL, a.data.flags}},
                         DbCondition(IDX_COL, DBCT::EQ, vit->idx));
        // update our cached version
        new_attributes[a.predicate].push_back(
            IndexedAttributeData{vit->idx, a.data});
        new_pos++;
      } else {
        // delete
        // Delete the remainder in the database
        for (auto dit = vit; dit != it.second.end(); ++dit) {
          LOG_DEBUG << "Deleting attribute " << it.first << LOG_END;
          _storage->erase(DbCondition(IDX_COL, DBCT::EQ, dit->idx));
        }
        break;
      }
    }
  }
  for (size_t i = new_pos; i < attributes.size(); ++i) {
    LOG_DEBUG << "Adding a new attribute " << attributes[i].predicate
              << LOG_END;
    const Attribute &a = attributes[i];
    // create
    int64_t idx = writeAttribute(a.predicate, a.data);
    new_attributes[a.predicate].push_back({idx, a.data});
  }

  // update the cache
  _attributes = new_attributes;

  // update our inherited attributes.
  updateInheritedAttributes();
}

void Wiki::Entry::setAttribute(const std::string &predicate,
                               const IndexedAttributeData *d,
                               const AttributeData &new_value) {
  auto it = _attributes.find(predicate);
  if (it != _attributes.end()) {
    for (IndexedAttributeData &od : it->second) {
      if (od.idx == d->idx) {
        bool updateInherited = od.data.flags & ATTR_INHERITABLE;
        updateInherited |= new_value.flags & ATTR_INHERITABLE;
        od.data = new_value;
        // write the changes to disk
        updateAttribute(od.idx, predicate, od.data);
        if (updateInherited) {
          for (Entry *e : _children) {
            e->updateInheritedAttributes();
          }
        }
      }
    }
  }
}

int64_t Wiki::Entry::writeAttribute(const std::string &predicate,
                                    const AttributeData &value) {
  _storage->insert({{ID_COL, _id},
                    {PREDICATE_COL, predicate},
                    {VALUE_COL, value.value},
                    {FLAG_COL, value.flags}});
  DbCursor c =
      _storage->query(DbCondition(ID_COL, DBCT::EQ, _id) &&
                      DbCondition(PREDICATE_COL, DBCT::EQ, predicate) &&
                      DbCondition(VALUE_COL, DBCT::EQ, value.value));
  if (c.done()) {
    return -1;
  }
  return c.col(0).integer;
}

void Wiki::Entry::updateAttribute(int64_t idx, const std::string &new_predicate,
                                  const AttributeData &new_value) {
  _storage->update({{ID_COL, _id},
                    {PREDICATE_COL, new_predicate},
                    {VALUE_COL, new_value.value},
                    {FLAG_COL, new_value.flags}},
                   DbCondition(IDX_COL, DBCT::EQ, idx));
}

void Wiki::Entry::removeAttribute(const std::string &predicate) {
  if (_attributes.find(predicate) != _attributes.end()) {
    _attributes.erase(predicate);
    _storage->erase(DbCondition(ID_COL, DBCT::EQ, _id) &&
                    DbCondition(PREDICATE_COL, DBCT::EQ, predicate));
    for (Entry *e : _children) {
      e->updateInheritedAttributes();
    }
  }
}

void Wiki::Entry::removeAttribute(const std::string &predicate,
                                  const std::string &value) {
  auto it = _attributes.find(predicate);
  bool updateInherited = false;
  if (it != _attributes.end()) {
    IndexedAttributeData d;
    d.data.value = value;
    // Figure out which entries to remove.
    auto sit = std::remove(it->second.begin(), it->second.end(), d);
    auto vit = sit;
    // erase the entries from the database
    while (vit != it->second.end()) {
      _storage->erase(DbCondition(IDX_COL, DBCT::EQ, vit->idx));
      if (vit->data.flags & ATTR_INHERITABLE) {
        updateInherited = true;
      }
      ++vit;
    }
    it->second.erase(sit, it->second.end());
  }
  if (updateInherited) {
    for (Entry *e : _children) {
      e->updateInheritedAttributes();
    }
  }
}

bool Wiki::Entry::hasAttribute(const std::string &predicate) const {
  return _attributes.count(predicate) > 0;
}

bool Wiki::Entry::hasAttribute(const std::string &predicate,
                               const std::string &value) const {
  auto it = _attributes.find(predicate);
  IndexedAttributeData d;
  d.data.value = value;
  if (it != _attributes.end()) {
    return std::find(it->second.begin(), it->second.end(), d) !=
           it->second.end();
  }
  return false;
}

const std::unordered_map<std::string, std::vector<Wiki::IndexedAttributeData>>
    &Wiki::Entry::inherited_attributes() const {
  return _inherited_attributes;
}

void Wiki::Entry::updateInheritedAttributes() {
  std::vector<Entry *> to_process;
  to_process.push_back(this);
  while (!to_process.empty()) {
    Entry *e = to_process.back();
    to_process.pop_back();
    e->_inherited_attributes.clear();
    if (e->_parent != nullptr) {
      // inherited the attributes our parent inherited
      for (const auto &it : e->_parent->_inherited_attributes) {
        if (e->_attributes.find(it.first) == e->_attributes.end()) {
          e->_inherited_attributes.insert(it);
        }
      }
      // also inherit the parent inheritable attributes
      for (const auto &ait : e->_parent->_attributes) {
        if (e->_attributes.find(ait.first) == e->_attributes.end()) {
          for (const IndexedAttributeData &d : ait.second) {
            if (d.data.flags & ATTR_INHERITABLE) {
              e->_inherited_attributes[ait.first].push_back(d);
            }
          }
        }
      }
    }
    to_process.insert(to_process.end(), e->_children.begin(),
                      e->_children.end());
  }
}

// =============================================================================
// Entry
// =============================================================================

Wiki::Date::Date() : _fields_used(0) {}

Wiki::Date::Date(const std::string &s) { parse(s); }

Wiki::Date &Wiki::Date::operator=(const Date &other) {
  _fields_used = other._fields_used;
  for (size_t i = 0; i < _fields_used; ++i) {
    _fields[i] = other._fields[i];
    if (i + 1 < _fields_used) {
      _delimiters[i] = other._delimiters[i];
    }
  }
  return *this;
}

bool Wiki::Date::operator==(const Date &other) const {
  if (_fields_used != other._fields_used) {
    return false;
  }
  for (size_t i = 0; i < _fields_used; ++i) {
    if (_fields[i] != other._fields[i]) {
      return false;
    }
  }
  return true;
}

bool Wiki::Date::operator<(const Date &other) const {
  size_t min_fields = std::min(_fields_used, other._fields_used);
  for (size_t i = 0; i < min_fields; ++i) {
    if (_fields[i] < other._fields[i]) {
      return true;
    }
    if (_fields[i] > other._fields[i]) {
      return false;
    }
  }
  // All fields we can compare equal
  return _fields_used < other._fields_used;
}

std::string Wiki::Date::toString() const {
  std::ostringstream os;
  for (size_t i = 0; i < _fields_used; ++i) {
    os << _fields[i];
    if (i + 1 < _fields_used) {
      os << _delimiters[i];
    }
  }
  return os.str();
}

int Wiki::Date::firstDifferentField(const Date &other) const {
  size_t min_fields = std::min(_fields_used, other._fields_used);
  if (min_fields == 0) {
    return 0;
  }
  for (size_t i = 0; i < min_fields; ++i) {
    if (_fields[i] != other._fields[i]) {
      return i;
    }
  }
  return _delimiters.size();
}

void Wiki::Date::parse(const std::string &s) {
  size_t pos = 0;
  // skip leading whitespace
  while (pos < s.size() && std::isspace(s[pos])) {
    pos++;
  }

  size_t field_idx = 0;
  size_t start = pos;
  while (pos < s.size()) {
    char c = s[pos];
    if (c == ':' || c == ' ' || c == '+' || c == '-' || c == '/' || c == '.') {
      _fields[field_idx] = std::stoi(s.substr(start, pos - start));
      if (field_idx < _delimiters.size()) {
        // We have space for one delimiter less than fields, as a delimiter
        // after the last field doesn't make any sense
        _delimiters[field_idx] = c;
      }
      field_idx++;
      start = pos + 1;
      if (field_idx >= _fields.size()) {
        throw std::runtime_error("Dates may only contain up to " +
                                 std::to_string(_fields.size()) + " fields.");
      }
    }
    pos++;
  }
  // Add the last field
  _fields[field_idx] = std::stoi(s.substr(start));
  _fields_used = field_idx + 1;
}
