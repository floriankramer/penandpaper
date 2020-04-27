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
  // Build the entry tree
  DbCursor c = _pages_table.query();
  // initially create a list of entries
  while (!c.done()) {
    std::string id = c.col(1).text;
    if (_entry_map.count(id) == 0) {
      _entry_map.insert(
          std::make_pair(id, new Entry(id, nullptr, &_pages_table)));
    }
    c.next();
  }
  // Then build the tree and assign all attributes
  c.reset();
  std::vector<int64_t> duplicates_to_erase;
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
            LOG_ERROR << "The entry " << id
                      << " refers to an nonexistant parent " << value
                      << LOG_END;
          }
        }
      }
      IndexedAttributeData d;
      d.idx = idx;
      d.data.value = value;
      d.data.flags = flags;
      if (!it->second->loadAttribute(predicate, d)) {
        duplicates_to_erase.push_back(idx);
      }
    } else {
      LOG_ERROR << "Wiki table modified during wiki loading." << LOG_END;
    }
    c.next();
  }

  // Delete dupliacte entries in the database. According to the spec they can't
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
  std::string cache_key = id + ":" + TEXT_ATTR;
  if (it != _entry_map.end()) {
    json entry;
    for (auto &ait : it->second->attributes()) {
      for (const IndexedAttributeData &a : ait.second) {
        json attr;
        attr["predicate"] = ait.first;
        if (ait.first == TEXT_ATTR) {
          if (_markdown_cache.count(cache_key) > 0) {
            attr["value"] = _markdown_cache[cache_key];
          } else {
            std::string raw;
            const std::vector<IndexedAttributeData> *attrs =
                it->second->getAttribute(TEXT_ATTR);
            if (attrs != nullptr && !attrs->empty()) {
              raw = attrs->at(0).data.value;
            } else {
              LOG_WARN << "Entry " << id << " has no " << TEXT_ATTR << LOG_END;
            }
            Markdown m(raw,
                       std::bind(&Wiki::lookupAttribute, this,
                                 std::placeholders::_1, std::placeholders::_2));
            try {
              std::string parsed = m.process();
              if (_markdown_cache.size() > MAX_MARKDOWN_CACHE_SIZE) {
                // just erase any element.
                _markdown_cache.erase(_markdown_cache.begin());
              }
              _markdown_cache[cache_key] = parsed;
              attr["value"] = parsed;
            } catch (const std::exception &e) {
              LOG_WARN << "Error while parsing the markdown: " << e.what()
                       << LOG_END;
              resp.status = 500;
              resp.body = "Unable to parse the input markdown<br/>" + raw;
              return;
            }
          }
        } else {
          attr["value"] = a.data.value;
        }
        attr["isInteresting"] = (a.data.flags & ATTR_INTERESTING) > 0;
        attr["isInheritable"] = (a.data.flags & ATTR_INHERITABLE) > 0;
        attr["isDate"] = (a.data.flags & ATTR_DATE) > 0;
        entry.push_back(attr);
      }
    }
    resp.status = 200;
    resp.body = entry.dump();
  } else {
    resp.status = 404;
    resp.body = "No such wiki entry";
  }
}

void Wiki::handleRaw(const std::string &id, httplib::Response &resp) {
  using nlohmann::json;
  auto it = _entry_map.find(id);
  if (it != _entry_map.end()) {
    json entry;
    for (auto &ait : it->second->attributes()) {
      for (const IndexedAttributeData &a : ait.second) {
        json attr;
        attr["predicate"] = ait.first;
        attr["value"] = a.data.value;
        attr["isInteresting"] = (a.data.flags & ATTR_INTERESTING) > 0;
        attr["isInheritable"] = (a.data.flags & ATTR_INHERITABLE) > 0;
        attr["isDate"] = (a.data.flags & ATTR_DATE) > 0;
        entry.push_back(attr);
      }
    }
    resp.status = 200;
    resp.body = entry.dump();
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
      it->second->setAttributes(attributes);
      if (parent != it->second->parent()) {
        it->second->reparent(parent);
      }
      addToSearchIndex(it->second);
    } else {
      Entry *e = parent->addChild(id);
      e->setAttributes(attributes);
      _entry_map[id] = e;
      addToSearchIndex(e);
    }
    // This is a very aggressive cache invalidation strategy. It is used, as it
    // is simple and ensures all attribute refs are recomputed.
    _markdown_cache.clear();
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
  }

  // This will recursively free the memory of the children
  delete it->second;
  resp.status = 200;
  resp.body = "Deletion succesfull";
  return;
}

void Wiki::autoLink(Entry *e, double score_threshold) {
  const auto *texts = e->getAttribute(TEXT_ATTR);
  if (texts == nullptr || texts->empty()) {
    return;
  }
  for (const IndexedAttributeData &data : *texts) {
    const std::string &text = data.data.value;
    std::ostringstream result;
    // a vector of alternating words and whitespace
    std::vector<std::string> ctx_entries;
    // TODO: keep between 16 and 32 characters worth of data in ctx_entries.
    // then whenever a full word was read apply the autocompletion. Apply any
    // results with score at least score_threshold. if nothing was applied throw
    // out the last two entries in the vector and write them to result. Ignore
    // any data inside of () or [] and discard the current context if such a
    // block is found.

    // Update the cache, invalidate the markdown cache, write to disk
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
    return (*vals)[0].data.value;
  } else {
    std::ostringstream out;
    for (size_t i = 0; i < vals->size(); ++i) {
      out << vals->at(i).data.value;
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

// =============================================================================
// Entry
// =============================================================================

Wiki::Entry::Entry(Table *storage)
    : _id("root"), _parent(nullptr), _storage(storage) {}

Wiki::Entry::Entry(const std::string &id, Entry *parent, Table *storage)
    : _id(id), _parent(parent), _storage(storage) {
  if (_parent != nullptr) {
    _parent->_children.push_back(this);
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
  }
}

void Wiki::Entry::removeAttribute(const std::string &predicate,
                                  const std::string &value) {
  auto it = _attributes.find(predicate);
  if (it != _attributes.end()) {
    IndexedAttributeData d;
    d.data.value = value;
    // Figure out which entries to remove.
    auto sit = std::remove(it->second.begin(), it->second.end(), d);
    auto vit = sit;
    // erase the entries from the database
    while (vit != it->second.end()) {
      _storage->erase(DbCondition(IDX_COL, DBCT::EQ, vit->idx));
      ++vit;
    }
    it->second.erase(sit, it->second.end());
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
