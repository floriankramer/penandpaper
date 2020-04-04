#include "Database.h"

#include <sstream>

#include "Logger.h"

std::string dbDataTypeName(DbDataType t) {
  switch (t) {
    case DbDataType::NULL_T:
      return "NULL";
    case DbDataType::BLOB:
      return "BLOB";
    case DbDataType::REAL:
      return "REAL";
    case DbDataType::INTEGER:
      return "INTEGER";
    case DbDataType::TEXT:
      return "TEXT";
  }
  return "";
}

// =============================================================================
// DbVariant
// =============================================================================

DbVariant::DbVariant() : type(DbDataType::INTEGER), integer(0) {}

DbVariant::~DbVariant() {
  if (type == DbDataType::BLOB) {
    (this->blob).std::vector<uint8_t>::~vector();
  } else if (type == DbDataType::TEXT) {
    (this->text).std::string::~basic_string();
  }
}

DbVariant::DbVariant(const DbVariant &other) : type(other.type) {
  switch (type) {
    case DbDataType::BLOB:
      blob = other.blob;
      break;
    case DbDataType::REAL:
      real = other.real;
      break;
    case DbDataType::TEXT:
      text = other.text;
      break;
    case DbDataType::NULL_T:
      integer = 0;
      break;
    case DbDataType::INTEGER:
      integer = other.integer;
      break;
  }
}

DbVariant &DbVariant::operator=(const DbVariant &other) {
  this->DbVariant::~DbVariant();
  new (this) DbVariant(other);
  return *this;
}

// =============================================================================
// DbCursor
// =============================================================================

DbCursor::DbCursor(sqlite3_stmt *stmt, sqlite3 *db)
    : _stmt(stmt), _db(db), _done(false) {
  // load the first row
  next();
}

DbCursor::~DbCursor() { sqlite3_finalize(_stmt); }

void DbCursor::next() {
  int r = sqlite3_step(_stmt);
  if (r == SQLITE_DONE) {
    _done = true;
  } else if (r != SQLITE_OK) {
    LOG_ERROR << "Unable to step a db cursor: " << sqlite3_errstr(r) << LOG_END;
  }
}
bool DbCursor::done() const { return _done; }

DbVariant DbCursor::col(int index) {
  DbVariant v;
  sqlite3_value *val = sqlite3_column_value(_stmt, index);
  int type = sqlite3_value_type(val);
  switch (type) {
    case SQLITE_INTEGER:
      v.type = DbDataType::INTEGER;
      v.integer = sqlite3_value_int(val);
      break;
    case SQLITE_NULL:
      v.type = DbDataType::NULL_T;
      v.integer = 0;
      break;
    case SQLITE_FLOAT:
      v.type = DbDataType::REAL;
      v.real = sqlite3_value_double(val);
      break;
    case SQLITE_TEXT: {
      v.type = DbDataType::TEXT;
      int size = sqlite3_value_bytes(val);
      v.text = std::string(
          reinterpret_cast<const char *>(sqlite3_value_text(val), size));
    } break;
    case SQLITE_BLOB: {
      v.type = DbDataType::BLOB;
      int size = sqlite3_value_bytes(val);
      const uint8_t *data =
          reinterpret_cast<const uint8_t *>(sqlite3_value_blob(val));
      v.blob = std::vector<uint8_t>(data, data + size);
    } break;
  }
  return v;
}

// =============================================================================
// Table
// =============================================================================

Table::Table(const std::string &name, sqlite3 *db) : _name(name), _db(db) {}

Table::~Table() {}

void Table::setColumns(const std::vector<DbColumn> &columns) {
  _columns = columns;
}

void Table::insert(const std::vector<DbVariant> &data) {
  std::stringstream ssql;
  ssql << "INSERT INTO " << _name << " VALUES (";
  for (size_t i = 0; i < data.size(); ++i) {
    ssql << "?";
    if (i + 1 < data.size()) {
      ssql << ", ";
    }
  }
  ssql << ");";

  std::string sql = ssql.str();
  sqlite3_stmt *stmt;
  int r = sqlite3_prepare_v2(_db, sql.c_str(), sql.size(), &stmt, NULL);
  if (r != SQLITE_OK) {
    LOG_ERROR << "Unable to prepare an sqlite statement for insertion into "
              << _name << ": " << sql << LOG_END;
    return;
  }
  // Bind the values
  for (size_t i = 0; i < data.size(); ++i) {
    const DbVariant &v = data[i];
    switch (v.type) {
      case DbDataType::BLOB:
        // TODO: transient is safe but expensive as it copies the data
        r = sqlite3_bind_blob(stmt, i, v.blob.data(), v.blob.size(),
                              SQLITE_TRANSIENT);
        break;
      case DbDataType::REAL:
        r = sqlite3_bind_double(stmt, i, v.real);
        break;
      case DbDataType::TEXT:
        r = sqlite3_bind_text(stmt, i, v.text.c_str(), v.text.size(),
                              SQLITE_TRANSIENT);
        break;
      case DbDataType::NULL_T:
        r = sqlite3_bind_null(stmt, i);
        break;
      case DbDataType::INTEGER:
        r = sqlite3_bind_int64(stmt, i, v.integer);
        break;
    }
    if (r != SQLITE_OK) {
      LOG_ERROR << "Unable to bind data slot " << i << " for insertion into "
                << _name << ": " << sql << LOG_END;
      return;
    }
  }
  r = sqlite3_step(stmt);
  if (r != SQLITE_OK) {
    LOG_ERROR << "Unable to insert into " << _name << ": " << sql << LOG_END;
    return;
  }
  r = sqlite3_finalize(stmt);
  if (r != SQLITE_OK) {
    LOG_ERROR << "Unable to finalize the statement for insertion into " << _name
              << ": " << sql << LOG_END;
    return;
  }
}

void Table::erase(const std::string &where) {
  std::stringstream ssql;
  ssql << "DELETE FROM " << _name << " WHERE " << where << ";";

  std::string sql = ssql.str();
  char *error = NULL;
  sqlite3_exec(_db, sql.c_str(), NULL, NULL, &error);
  if (error != NULL) {
    LOG_ERROR << "Error while deleting from: " << _name << ": " << sql << " :"
              << error << LOG_END;
    sqlite3_free(error);
  }
}

DbCursor Table::query(const std::string &where) {
  std::stringstream ssql;
  ssql << "SELECT * FROM " << _name;
  if (where.size() > 0) {
    ssql << " WHERE " << where;
  }
  ssql << ";";

  std::string sql = ssql.str();
  sqlite3_stmt *stmt;
  int r = sqlite3_prepare_v2(_db, sql.c_str(), sql.size(), &stmt, NULL);
  if (r != SQLITE_OK) {
    LOG_ERROR << "Unable to prepare an sqlite statement for query from "
              << _name << ": " << sql << LOG_END;
  }
  return DbCursor(stmt, _db);
}

// =============================================================================
// DATABASE
// =============================================================================
Database::Database(const std::string &database_path) {
  int r = sqlite3_open(database_path.c_str(), &_db);
  if (r != SQLITE_OK) {
    _db == nullptr;
    LOG_ERROR << "Unable to open the database at " << database_path << LOG_END;
  }
}

Table Database::createTable(const std::string &name,
                            const std::vector<DbColumn> &types) {
  std::stringstream ssql;
  ssql << "CREATE TABLE IF NOT EXISTS " << name << " (";
  for (size_t i = 0; i < types.size(); ++i) {
    ssql << types[i].name << " " << dbDataTypeName(types[i].type);
    if (i + 1 < types.size()) {
      ssql << ", ";
    }
  }
  ssql << ");";

  std::string sql = ssql.str();
  char *error = NULL;
  sqlite3_exec(_db, sql.c_str(), NULL, NULL, &error);
  if (error != NULL) {
    LOG_ERROR << "Error while creating a table: " << error << LOG_END;
    sqlite3_free(error);
  }
  Table table(name, _db);
  table.setColumns(types);
  return table;
}

Database::~Database() {
  if (_db != nullptr) {
    sqlite3_close(_db);
  }
}
