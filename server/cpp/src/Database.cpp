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
DbVariant::DbVariant() : type(DbDataType::NULL_T), integer(0) {}

DbVariant::DbVariant(int64_t integer)
    : type(DbDataType::INTEGER), integer(integer) {}
DbVariant::DbVariant(double real) : type(DbDataType::REAL), real(real) {}
DbVariant::DbVariant(const std::string &text)
    : type(DbDataType::TEXT), text(text) {}
DbVariant::DbVariant(const std::vector<uint8_t> &blob)
    : type(DbDataType::BLOB), blob(blob) {}

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
      new (this) DbVariant(other.blob);
      break;
    case DbDataType::REAL:
      new (this) DbVariant(other.real);
      break;
    case DbDataType::TEXT:
      new (this) DbVariant(other.text);
      break;
    case DbDataType::NULL_T:
      new (this) DbVariant();
      break;
    case DbDataType::INTEGER:
      new (this) DbVariant(other.integer);
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

DbCursor::DbCursor() : _db(NULL), _stmt(NULL), _done(true) {}

DbCursor::DbCursor(sqlite3_stmt *stmt, sqlite3 *db)
    : _stmt(stmt), _db(db), _done(false) {
  // load the first row
  next();
}

DbCursor::~DbCursor() { sqlite3_finalize(_stmt); }

void DbCursor::next() {
  int r = sqlite3_step(_stmt);
  if (r != SQLITE_ROW) {
    _done = true;
  }
  if (r != SQLITE_OK && r != SQLITE_ROW && r != SQLITE_DONE) {
    LOG_ERROR << "Unable to step a db cursor: " << sqlite3_errstr(r) << LOG_END;
    _done = true;
  }
}
bool DbCursor::done() const { return _done; }

DbVariant DbCursor::col(int index) {
  sqlite3_value *val = sqlite3_column_value(_stmt, index);
  int type = sqlite3_value_type(val);
  switch (type) {
    case SQLITE_INTEGER:
      return int64_t(sqlite3_value_int(val));
    case SQLITE_NULL:
      return DbVariant();
    case SQLITE_FLOAT:
      return sqlite3_value_double(val);
    case SQLITE_TEXT: {
      int size = sqlite3_value_bytes(val);
      return std::string(
          reinterpret_cast<const char *>(sqlite3_value_text(val)), size);
    }
    case SQLITE_BLOB: {
      int size = sqlite3_value_bytes(val);
      const uint8_t *data =
          reinterpret_cast<const uint8_t *>(sqlite3_value_blob(val));
      return std::vector<uint8_t>(data, data + size);
    }
    default:
      return DbVariant();
  }
}

// =============================================================================
// DbCondition
// =============================================================================

DbCondition::DbCondition() : column(), type(DbCondition::Type::ALL), value() {}
DbCondition::DbCondition(const std::string &column, Type type,
                         const DbVariant &value)
    : column(column), type(type), value(value) {}

DbCondition DbCondition::operator&&(const DbCondition &other) {
  DbCondition merged;
  merged.type = DbCondition::Type::AND;
  merged.children.push_back(*this);
  merged.children.push_back(other);
  return merged;
}

DbCondition DbCondition::operator||(const DbCondition &other) {
  DbCondition merged;
  merged.type = DbCondition::Type::OR;
  merged.children.push_back(*this);
  merged.children.push_back(other);
  return merged;
}

DbCondition DbCondition::operator!() {
  DbCondition merged;
  merged.type = DbCondition::Type::NOT;
  merged.children.push_back(*this);
  return merged;
}

DbSqlBuilder &operator<<(DbSqlBuilder &builder, const DbCondition &c) {
  if (c.type == DbCondition::Type::ALL) {
    return builder;
  }
  if (c.type == DbCondition::Type::AND) {
    builder << "(";
    for (size_t i = 0; i < c.children.size(); ++i) {
      builder << c.children[i];
      if (i + 1 < c.children.size()) {
        builder << " AND ";
      }
    }
    builder << ")";
  } else if (c.type == DbCondition::Type::OR) {
    builder << "(";
    for (size_t i = 0; i < c.children.size(); ++i) {
      builder << c.children[i];
      if (i + 1 < c.children.size()) {
        builder << " OR ";
      }
    }
    builder << ")";
  } else if (c.type == DbCondition::Type::NOT) {
    builder << "(NOT " << c.children[0] << ")";
  } else {
    builder << c.column;
    switch (c.type) {
      case DbCondition::Type::EQ:
        builder << " = ";
        break;
      case DbCondition::Type::GT:
        builder << " > ";
        break;
      case DbCondition::Type::LT:
        builder << " < ";
        break;
      case DbCondition::Type::GE:
        builder << " >= ";
        break;
      case DbCondition::Type::LE:
        builder << " <= ";
        break;
    }
    builder << c.value;
  }
  return builder;
}

// =============================================================================
// Table
// =============================================================================

DbSqlBuilder &DbSqlBuilder::operator<<(const std::string &s) {
  _str << s;
  return *this;
}
DbSqlBuilder &DbSqlBuilder::operator<<(const DbVariant &v) {
  _str << "?" << _data.size() + 1;
  _data.push_back(v);
  return *this;
}

DbSqlBuilder &DbSqlBuilder::operator<<(const DbColumnUpdate &v) {
  *this << v.name << " = " << v.data;
  return *this;
}

std::string DbSqlBuilder::str() const { return _str.str(); }

const std::vector<DbVariant> &DbSqlBuilder::data() const { return _data; }

// =============================================================================
// Table
// =============================================================================

Table::Table(const std::string &name, sqlite3 *db) : _name(name), _db(db) {}

Table::~Table() {}

void Table::setColumns(const std::vector<DbColumn> &columns) {
  _columns = columns;
}

void Table::insert(const std::vector<DbVariant> &data) {
  DbSqlBuilder ssql;
  ssql << "INSERT INTO " << _name << " VALUES (";
  for (size_t i = 0; i < data.size(); ++i) {
    ssql << data[i];
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
              << _name << ": " << sql << "\n"
              << sqlite3_errmsg(_db) << LOG_END;
    return;
  }

  r = bindValues(stmt, ssql);
  if (r != SQLITE_OK) {
    LOG_ERROR << "Unable to bind values for insertion into " << _name << ": "
              << sql << "\n"
              << sqlite3_errmsg(_db) << LOG_END;
    return;
  }

  r = sqlite3_step(stmt);
  if (r != SQLITE_OK && r != SQLITE_DONE) {
    LOG_ERROR << "Unable to insert into " << _name << ": " << sql << "\n"
              << sqlite3_errmsg(_db) << LOG_END;
    return;
  }
  r = sqlite3_finalize(stmt);
  if (r != SQLITE_OK) {
    LOG_ERROR << "Unable to finalize the statement for insertion into " << _name
              << ": " << sql << "\n"
              << sqlite3_errmsg(_db) << LOG_END;
    return;
  }
}

void Table::erase(const DbCondition &where) {
  DbSqlBuilder ssql;
  ssql << "DELETE FROM " << _name << " WHERE " << where << ";";

  std::string sql = ssql.str();
  sqlite3_stmt *stmt;
  int r = sqlite3_prepare_v2(_db, sql.c_str(), sql.size(), &stmt, NULL);
  if (r != SQLITE_OK) {
    LOG_ERROR << "Unable to prepare an sqlite statement for insertion into "
              << _name << ": " << sql << "\n"
              << sqlite3_errmsg(_db) << LOG_END;
    return;
  }
  // Bind the values
  r = bindValues(stmt, ssql);
  if (r != SQLITE_OK) {
    LOG_ERROR << "Unable to bind values for insertion into " << _name << ": "
              << sql << "\n"
              << sqlite3_errmsg(_db) << LOG_END;
    return;
  }

  r = sqlite3_step(stmt);
  if (r != SQLITE_OK && r != SQLITE_DONE) {
    LOG_ERROR << "Unable to update " << _name << ": " << sql << "\n"
              << sqlite3_errmsg(_db) << LOG_END;
    return;
  }
  r = sqlite3_finalize(stmt);
  if (r != SQLITE_OK) {
    LOG_ERROR << "Unable to finalize the statement for updating " << _name
              << ": " << sql << "\n"
              << sqlite3_errmsg(_db) << LOG_END;
    return;
  }
}

DbCursor Table::query(const DbCondition &where) {
  DbSqlBuilder ssql;
  ssql << "SELECT * FROM " << _name;
  if (where.type != DbCondition::Type::ALL) {
    ssql << " WHERE " << where;
  }
  ssql << ";";

  std::string sql = ssql.str();

  sqlite3_stmt *stmt;
  int r = sqlite3_prepare_v2(_db, sql.c_str(), sql.size(), &stmt, NULL);
  if (r != SQLITE_OK) {
    LOG_ERROR << "Unable to prepare an sqlite statement for insertion into "
              << _name << ": " << sql << "\n"
              << sqlite3_errmsg(_db) << LOG_END;
    return DbCursor();
  }
  // Bind the values
  r = bindValues(stmt, ssql);
  if (r != SQLITE_OK) {
    LOG_ERROR << "Unable to bind values for insertion into " << _name << ": "
              << sql << "\n"
              << sqlite3_errmsg(_db) << LOG_END;
    return DbCursor();
  }
  return DbCursor(stmt, _db);
}

void Table::update(const std::vector<DbColumnUpdate> &updates,
                   const DbCondition &where) {
  DbSqlBuilder ssql;
  ssql << "UPDATE " << _name << " SET ";
  for (size_t i = 0; i < updates.size(); ++i) {
    const DbColumnUpdate &c = updates[i];
    ssql << c;
    if (i + 1 < updates.size()) {
      ssql << ", ";
    }
  }
  ssql << " WHERE " << where << ";";

  std::string sql = ssql.str();
  sqlite3_stmt *stmt;
  int r = sqlite3_prepare_v2(_db, sql.c_str(), sql.size(), &stmt, NULL);
  if (r != SQLITE_OK) {
    LOG_ERROR << "Unable to prepare an sqlite statement for insertion into "
              << _name << ": " << sql << "\n"
              << sqlite3_errmsg(_db) << LOG_END;
    return;
  }
  // Bind the values
  r = bindValues(stmt, ssql);
  if (r != SQLITE_OK) {
    LOG_ERROR << "Unable to bind values for insertion into " << _name << ": "
              << sql << "\n"
              << sqlite3_errmsg(_db) << LOG_END;
    return;
  }

  r = sqlite3_step(stmt);
  if (r != SQLITE_OK && r != SQLITE_DONE) {
    LOG_ERROR << "Unable to update " << _name << ": " << sql << "\n"
              << sqlite3_errmsg(_db) << LOG_END;
    return;
  }
  r = sqlite3_finalize(stmt);
  if (r != SQLITE_OK) {
    LOG_ERROR << "Unable to finalize the statement for updating " << _name
              << ": " << sql << "\n"
              << sqlite3_errmsg(_db) << LOG_END;
    return;
  }
}

int Table::bindValues(sqlite3_stmt *stmt, const DbSqlBuilder &builder) {
  for (size_t i = 0; i < builder.data().size(); ++i) {
    int r = bindValue(stmt, i + 1, builder.data()[i]);
    if (r != 0) {
      return r;
    }
  }
  return 0;
}

int Table::bindValue(sqlite3_stmt *stmt, int index, const DbVariant &value) {
  int r = 0;
  switch (value.type) {
    case DbDataType::BLOB:
      // TODO: transient is safe but expensive as it copies the data
      r = sqlite3_bind_blob(stmt, index, value.blob.data(), value.blob.size(),
                            SQLITE_TRANSIENT);
      break;
    case DbDataType::REAL:
      r = sqlite3_bind_double(stmt, index, value.real);
      break;
    case DbDataType::TEXT:
      r = sqlite3_bind_text(stmt, index, value.text.c_str(), value.text.size(),
                            SQLITE_TRANSIENT);
      break;
    case DbDataType::NULL_T:
      r = sqlite3_bind_null(stmt, index);
      break;
    case DbDataType::INTEGER:
      r = sqlite3_bind_int64(stmt, index, value.integer);
      break;
    default:
      r = SQLITE_ERROR;
  }
  return r;
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
