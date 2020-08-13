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
#ifndef DATABASE_H
#define DATABASE_H

#include <sqlite3.h>

#include <string>
#include <vector>
#include <sstream>

enum class DbDataType { NULL_T, INTEGER, REAL, TEXT, BLOB, AUTO_INCREMENT };

std::string dbDataTypeName(DbDataType t);

class DbSqlBuilder;

class DbVariant {
 public:
  DbVariant();
  DbVariant(int64_t integer);
  DbVariant(double real);
  DbVariant(const std::string &text);
  DbVariant(const std::vector<uint8_t> &blob);
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

class DbColumnUpdate {
 public:
  std::string name;
  DbVariant data;
};

class DbCondition {
public:
  enum class Type {
    ALL,
    AND,
    OR,
    NOT,
    EQ,
    GT,
    LT,
    GE,
    LE
  };

  DbCondition();
  DbCondition(const std::string &column, Type type, const DbVariant &value);

  DbCondition operator&&(const DbCondition &other);
  DbCondition operator||(const DbCondition &other);
  DbCondition operator!();

  std::string column;
  Type type;
  DbVariant value;
  std::vector<DbCondition> children;

  friend DbSqlBuilder &operator<<(DbSqlBuilder &builder, const DbCondition &c);
};

using DBCT = DbCondition::Type;

class DbSqlBuilder {
public:
  DbSqlBuilder &operator<<(const std::string &s);
  DbSqlBuilder &operator<<(const DbVariant &v);
  DbSqlBuilder &operator<<(const DbColumnUpdate &v);

  std::string str() const;
  const std::vector<DbVariant> &data() const;

private:
  std::ostringstream _str;
  std::vector<DbVariant> _data;
};

class DbCursor {
 public:
  DbCursor();
  DbCursor(sqlite3_stmt *stmt, sqlite3 *db);
  virtual ~DbCursor();

  void next();
  bool done() const;
  DbVariant col(int index);

  void reset();

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

  void insert(const std::vector<DbColumnUpdate> &data);
  void insert(const std::vector<DbVariant> &data);
  void erase(const DbCondition &where);
  DbCursor query(const DbCondition &where = DbCondition());
  void update(const std::vector<DbColumnUpdate> &updates, const DbCondition &where);

 private:
  int bindValues(sqlite3_stmt *stmt, const DbSqlBuilder &builder);
  int bindValue(sqlite3_stmt *stmt, int index, const DbVariant &value);

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
