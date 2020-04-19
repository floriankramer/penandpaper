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
    int flags;
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

    const std::vector<AttributeData> *getAttribute(
        const std::string &predicate) const;

    /**
     * @brief Sets the attribute but does not write it to the persistent
     * storage. Meant to be used during loading.
     */
    void loadAttribute(const std::string &predicate, const AttributeData &value);

    void addAttribute(const std::string &predicate, const AttributeData &value);
    void setAttribute(const std::string &predicate,
                      const std::string &old_value,
                      const AttributeData &new_value);

    /**
      @brief Replaces the entire set of attributes with the given one
     **/
    void setAttributes(const std::vector<Attribute> &attributes);

    /**
     * @brief Sets all instances of this attr with the given predicate to the
     * new value
     */
    void setAttribute(const std::string &predicate,
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

   private:
    void writeAttribute(const std::string &predicate, const AttributeData &value);

    std::string _id;

    Entry *_parent;
    std::vector<Entry *> _children;

    std::unordered_map<std::string, std::vector<AttributeData>> _attributes;

    Table *_storage;
  };

 public:
  Wiki(Database *db);

  virtual void onRequest(const httplib::Request &req, httplib::Response &resp);

 private:
  void handleList(httplib::Response &resp);
  void handleGet(const std::string &id, httplib::Response &resp);
  void handleRaw(const std::string &id, httplib::Response &resp);
  void handleSave(const std::string &id, const httplib::Request &req,
                  httplib::Response &resp);
  void handleDelete(const std::string &id, const httplib::Request &req,
                    httplib::Response &resp);
  void handleCompleteEntity(const httplib::Request &req,
                            httplib::Response &resp);

  Database *_db;
  Table _pages_table;
  std::unordered_map<std::string, Entry *> _entry_map;
  std::unordered_map<std::string, std::string> _markdown_cache;

  QGramIndex _ids_search_index;

  Entry _root;

  static constexpr size_t MAX_MARKDOWN_CACHE_SIZE = 16;
};
