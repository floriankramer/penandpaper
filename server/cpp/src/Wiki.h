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
#pragma once

#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "Database.h"
#include "HttpServer.h"
#include "QGramIndex.h"

class Wiki : public HttpServer::RequestHandler {
  static const std::string IDX_COL;
  static const std::string ID_COL;
  static const std::string PREDICATE_COL;
  static const std::string VALUE_COL;
  static const std::string FLAG_COL;

  static const std::string TEXT_ATTR;

  static const int ATTR_INTERESTING;
  static const int ATTR_INHERITABLE;
  static const int ATTR_DATE;

  struct AttributeData {
    std::string value;
    int64_t flags;
    std::string value_markdown_html;

    bool operator==(const AttributeData &other) const {
      return other.value == value;
    }
  };

  /**
   * @brief Includes an index used for quicker sql queries
   */
  struct IndexedAttributeData {
    int64_t idx;
    AttributeData data;

    bool operator==(const IndexedAttributeData &other) const {
      return other.data == data;
    }
  };

  struct Attribute {
    std::string predicate;
    AttributeData data;
  };

  class Entry {
   public:
    Entry(Table *storage);
    Entry(const std::string &id, Entry *parent, Table *storage);
    virtual ~Entry();

    Entry *addChild(std::string child_id);

    const std::vector<IndexedAttributeData> *getAttribute(
        const std::string &predicate) const;

    /**
     * @brief Sets the attribute but does not write it to the persistent
     * storage. Meant to be used during loading. This does not update
     * inherited attributes. Call updateInheritedAttributes manually
     * on the root node after loading.
     * @return false if the attribute wasn't loaded because it already exists
     */
    bool loadAttribute(const std::string &predicate,
                       const IndexedAttributeData &value);

    void addAttribute(const std::string &predicate, const AttributeData &value);

    /**
      @brief Replaces the entire set of attributes with the given one
     **/
    void setAttributes(const std::vector<Attribute> &attributes);

    void setAttribute(const std::string &predicate,
                      const IndexedAttributeData *d,
                      const AttributeData &new_value);

    void removeAttribute(const std::string &predicate);
    void removeAttribute(const std::string &predicate,
                         const std::string &value);

    bool hasAttribute(const std::string &predicate) const;
    bool hasAttribute(const std::string &predicate,
                      const std::string &value) const;

    void reparent(Entry *new_parent);

    const std::string &name() const;
    const std::string &id() const;

    Entry *parent();
    const std::vector<Entry *> &children() const;

    const std::unordered_map<std::string, std::vector<IndexedAttributeData>>
        &attributes() const;

    const std::unordered_map<std::string, std::vector<IndexedAttributeData>>
        &inherited_attributes() const;

    void updateInheritedAttributes();

   private:
    int64_t writeAttribute(const std::string &predicate,
                           const AttributeData &value);
    void updateAttribute(int64_t idx, const std::string &new_predicate,
                         const AttributeData &new_value);

    std::string _id;

    Entry *_parent;
    std::vector<Entry *> _children;

    std::unordered_map<std::string, std::vector<IndexedAttributeData>>
        _attributes;

    std::unordered_map<std::string, std::vector<IndexedAttributeData>>
        _inherited_attributes;

    Table *_storage;
  };

 public:
  Wiki(Database *db);

  virtual void onRequest(const httplib::Request &req, httplib::Response &resp);

 private:
  std::string tryProcessMarkdown(const std::string &s);

  void handleList(httplib::Response &resp);
  void handleGet(const std::string &id, httplib::Response &resp);
  void handleRaw(const std::string &id, httplib::Response &resp);
  void handleSave(const std::string &id, const httplib::Request &req,
                  httplib::Response &resp);
  void handleDelete(const std::string &id, const httplib::Request &req,
                    httplib::Response &resp);
  void handleCompleteEntity(const httplib::Request &req,
                            httplib::Response &resp);
  void handleCompleteAttrRef(const httplib::Request &req,
                             httplib::Response &resp);
  void handleAutolink(const std::string &id, const httplib::Request &req,
                      httplib::Response &resp);
  void handleAutolinkAll(const httplib::Request &req, httplib::Response &resp);
  void handleContext(const std::string &id, httplib::Response &resp);

  // This scans the given entry and automatically references other entries
  // it finds in the entries text using entry autocompletion.
  void autoLink(Entry *e, double score_threshold = 0.95);

  // Used to resolve attribute links
  std::string lookupAttribute(const std::string &id,
                              const std::string &predicate);

  void removeFromSearchIndex(Entry *e);
  void addToSearchIndex(Entry *e);

  Database *_db;
  Table _pages_table;
  std::unordered_map<std::string, Entry *> _entry_map;

  QGramIndex _ids_search_index;
  QGramIndex _attr_ref_search_index;

  Entry _root;

  std::function<std::string(const std::string &, const std::string &)>
      _lookup_attributed_bound;

  static constexpr size_t MAX_MARKDOWN_CACHE_SIZE = 16;
};
