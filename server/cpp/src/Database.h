#ifndef DATABASE_H
#define DATABASE_H

#include <sqlite3.h>

#include <string>
#include <vector>

enum class DbDataType { NULL_T, INTEGER, REAL, TEXT, BLOB };

std::string dbDataTypeName(DbDataType t);

class DbVariant {
 public:
  DbVariant();
  virtual ~DbVariant();

  DbVariant(const DbVariant &other);

  DbVariant &operator=(const DbVariant &other);

  DbDataType type;
  union {
    int64_t integer;
    double real;
    std::string text;
    std::vector<uint8_t> blob;
  };
};

class DbColumn {
 public:
  std::string name;
  DbDataType type;
};

class DbCursor {
 public:
  DbCursor(sqlite3_stmt *stmt, sqlite3 *db);
  virtual ~DbCursor();

  void next();
  bool done() const;
  DbVariant col(int index);

 private:
  sqlite3 *_db;
  sqlite3_stmt *_stmt;
  bool _done;
};

class Table {
 public:
  Table(const std::string &name, sqlite3 *db);
  virtual ~Table();
  void setColumns(const std::vector<DbColumn> &columns);

  void insert(const std::vector<DbVariant> &data);
  void erase(const std::string &where);
  DbCursor query(const std::string &where = std::string());

 private:
  std::string _name;
  std::vector<DbColumn> _columns;
  sqlite3 *_db;
};

class Database {
 public:
  Database(const std::string &database_path);
  virtual ~Database();

  Table createTable(const std::string &name,
                    const std::vector<DbColumn> &types);

 private:
  sqlite3 *_db;
};

#endif  // DATABASE_H
