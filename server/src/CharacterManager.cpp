#include "CharacterManager.h"

#include "Logger.h"
#include "Util.h"

namespace penandpaper {

const std::string CharacterManager::COL_ID = "id";
const std::string CharacterManager::COL_USER_ID = "userid";
const std::string CharacterManager::COL_DATA = "data";

CharacterManager::CharacterManager(Database *db) : _db(db) {
  openDatabaseTable();
}

CharacterManager::~CharacterManager() {}

void CharacterManager::registerPluginFunctions(
    std::shared_ptr<PluginManager> plugin_manager) {
  plugin_manager->addApiFunction(
      "registerCharacterSheet",
      [this](LuaScript *script,
             std::vector<LuaScript::Variant> args) -> LuaScript::Variant {
        addCharacterSheetFromLua(script, args);

        // Return nil
        return LuaScript::Variant();
      },
      {LuaScript::Type::STRING, LuaScript::Type::TABLE});
}

void CharacterManager::addCharacterSheetFromLua(
    LuaScript *script, const std::vector<LuaScript::Variant> &args) {
  using Variant = LuaScript::Variant;
  if (args.size() != 2) {
    // This exits
    script->error(
        "registerCharacterSheet: Expected exactly two arguments but got " +
        std::to_string(args.size()));
  }
  // TODO: Without a reference to the lua script we can't call action functions.
  // We should probably receive a reference to the src script here, could also
  // consider changing the lua variant type to not store a copy of the data,
  // but instead to store a pointer to it (as far as possible, there might be
  // ephemeral data where that would not work, e.g. return values).
  CharacterSheet sheet;

  sheet.character_sheet_type = args[0].asString();
  LuaScript::Table blocks_table = args[1].asTable();

  for (auto block_it = blocks_table.vectorBegin();
       block_it != blocks_table.vectorEnd(); ++block_it) {
    Variant::VariantPtr src_block = *block_it;
    LuaScript::Table &block_table = src_block->asTable();

    AttributeBlock block;
    // TODO: Use 'at' functions for Attribute tables that throw if the key
    // doesn't exist.
    block.heading = (*block_table["heading"])->asString();
    block.width = 12;
    if (block_table["width"]) {
      block.width = (*block_table["width"])->asNumber();
    }

    LuaScript::Table attributes = (*block_table["attributes"])->asTable();
    for (auto attribute_it = attributes.stringBegin();
         attribute_it != attributes.stringEnd(); ++attribute_it) {
      switch (attribute_it->second->type()) {
        case LuaScript::Type::NUMBER:
          block.attributes.emplace_back(attribute_it->first,
                                        attribute_it->second->number());
          break;
        case LuaScript::Type::STRING:
          block.attributes.emplace_back(attribute_it->first,
                                        attribute_it->second->string());
          break;
        case LuaScript::Type::TABLE: {
          std::vector<Attribute> child_attrs;
          for (auto childattr_it = attribute_it->second->table().stringBegin();
               childattr_it != attribute_it->second->table().stringEnd();
               ++childattr_it) {
            switch (childattr_it->second->type()) {
              case LuaScript::Type::NUMBER:
                child_attrs.emplace_back(childattr_it->first,
                                         childattr_it->second->number());
                break;
              case LuaScript::Type::STRING:
                child_attrs.emplace_back(childattr_it->first,
                                         childattr_it->second->string());
                break;
              default:
                script->error(
                    "Only numbers and strings are valid types for "
                    "character "
                    "sheet list attributes (type of " +
                    attribute_it->first + "." + childattr_it->first + ").");
            }
          }
          block.attributes.emplace_back(attribute_it->first, child_attrs);
        } break;
        default:
          script->error(
              "Only numbers, strings and tables are valid types for "
              "character "
              "sheet attributes (type of " +
              attribute_it->first + ").");
      }
    }

    // TODO: iterate the actions

    sheet.blocks.push_back(block);
  }

  _character_sheet_templates.push_back(sheet);
}

nlohmann::json CharacterManager::characterSheetToJSON(
    const CharacterSheet &sheet) {
  using nlohmann::json;
  json data;
  data["type"] = sheet.character_sheet_type;
  data["name"] = sheet.character_name;

  json blocks = json::array();
  for (const AttributeBlock &block : sheet.blocks) {
    json block_j = json::object();
    block_j["heading"] = block.heading;
    block_j["width"] = block.width;

    json attributes = json::array();
    for (const Attribute &attr : block.attributes) {
      attributes.push_back(attributeToJSON(attr));
    }
    block_j["attributes"] = attributes;
    blocks.push_back(block_j);
  }
  data["blocks"] = blocks;

  // TODO: consider actions

  return data;
}

nlohmann::json CharacterManager::attributeToJSON(
    const CharacterManager::Attribute &attr) {
  using nlohmann::json;

  json attr_j = json::object();
  attr_j["name"] = attr.name();
  switch (attr.type()) {
    case AttributeType::LABEL:
      attr_j["type"] = "label";
      attr_j["value"] = nullptr;
      break;
    case AttributeType::NUMBER:
      attr_j["type"] = "number";
      attr_j["value"] = attr.number();
      break;
    case AttributeType::LIST: {
      attr_j["type"] = "list";
      // Encode the types
      json types_list_j = json::array();
      for (const Attribute &type : attr.list().types) {
        types_list_j.push_back(attributeToJSON(type));
      }
      attr_j["types"] = types_list_j;
      // Encode the values
      json values_list_j = json::array();
      for (const std::vector<Attribute> &inst : attr.list().values) {
        json inst_j = json::array();
        for (const Attribute &val : inst) {
          inst_j.push_back(attributeToJSON(val));
        }
        values_list_j.push_back(inst_j);
      }
      attr_j["values"] = values_list_j;
    } break;
    case AttributeType::STRING:
      attr_j["type"] = "string";
      attr_j["value"] = attr.string();
      break;
  }
  return attr_j;
}

CharacterManager::Attribute CharacterManager::attributeFromJSON(
    const nlohmann::json &j) {
  using nlohmann::json;
  std::string type = j.at("type");
  if (type == "label") {
    return CharacterManager::Attribute(j.at("name"));
  } else if (type == "number") {
    return CharacterManager::Attribute(j.at("name"),
                                       j.at("value").get<double>());
  } else if (type == "string") {
    return CharacterManager::Attribute(j.at("name"),
                                       j.at("value").get<std::string>());
  } else if (type == "list") {
    AttributeList<Attribute> list;
    for (const json &type : j.at("types")) {
      list.types.push_back(attributeFromJSON(type));
    }
    for (const json &row_j : j.at("values")) {
      std::vector<Attribute> row;
      row.reserve(list.types.size());
      for (const json &val : row_j) {
        row.push_back(attributeFromJSON(val));
      }
      list.values.emplace_back(std::move(row));
    }
    return CharacterManager::Attribute(j.at("name"), list.types, list.values);
  }
  throw std::runtime_error(
      "CharacterManager::attributeFromJSON: Unknown attribute type " + type);
}

CharacterManager::CharacterSheet CharacterManager::characterSheetFromJSON(
    const nlohmann::json &j) {
  using nlohmann::json;

  CharacterSheet sheet;
  sheet.character_name = j.at("name");
  sheet.character_sheet_type = j.at("type");

  for (const json &block_j : j.at("blocks")) {
    AttributeBlock block;
    block.heading = block_j.at("heading");
    block.width = block_j.at("width");
    for (const json &attr : block_j.at("attributes")) {
      block.attributes.emplace_back(attributeFromJSON(attr));
    }
    sheet.blocks.push_back(std::move(block));
    sheet.id = -1;
  }

  return sheet;
}

HttpServer::HttpResponse CharacterManager::onRequest(
    UserManager::UserPtr user, const HttpServer::HttpRequest &req) {
  using nlohmann::json;
  HttpServer::HttpResponse resp;

  std::vector<std::string> parts = util::splitString(req.path, '/');
  LOG_INFO << "CharacterManager "
           << (req.type == HttpServer::RequestType::GET ? "GET" : "POST")
           << " request for " << req.path << LOG_END;
  if (parts.size() < 2) {
    LOG_ERROR << "Invalid character manager request at path " << req.path
              << LOG_END;
    resp.setError(400, "Invalid character manager request.");
    return resp;
  }

  const std::string &action = parts[1];
  if (action == "list_types") {
    json data;

    json types = json::array();
    for (const CharacterSheet &c : _character_sheet_templates) {
      json cj;
      cj["type"] = c.character_sheet_type;
      types.push_back(cj);
    }
    data["types"] = types;

    resp.setBody(data.dump());
    resp.status_code = 200;
    resp.setMimeType("application/json");
  } else if (action == "get_type") {
    if (parts.size() != 3) {
      resp.setError(400, "Expected a three part path.");
    } else {
      bool sheet_exists = false;
      for (const CharacterSheet &c : _character_sheet_templates) {
        if (c.character_sheet_type == parts[2]) {
          json data = characterSheetToJSON(c);
          resp.setBody(data.dump());
          resp.status_code = 200;
          resp.setMimeType("application/json");
          sheet_exists = true;
          break;
        }
      }
      if (!sheet_exists) {
        resp.setError(400, "The requested character sheet " + parts[2] +
                               " does not exist");
      }
    }
  }

  return resp;
}

void CharacterManager::openDatabaseTable() {
  _table = _db->createTable("characters", {{COL_ID, DbDataType::AUTO_INCREMENT},
                                           {COL_USER_ID, DbDataType::INTEGER},
                                           {COL_DATA, DbDataType::TEXT}});
}

std::vector<CharacterManager::CharacterSheet>
CharacterManager::listUserCharacterSheets(int64_t user_id) {
  using nlohmann::json;

  std::vector<CharacterSheet> characters;
  DbCursor res =
      _table.query(DbCondition(COL_USER_ID, DbCondition::Type::EQ, user_id));
  while (!res.done()) {
    json raw = json::parse(res.col(2).asText());
    characters.emplace_back(characterSheetFromJSON(raw));
    res.next();
  }

  return characters;
}

std::optional<CharacterManager::CharacterSheet>
CharacterManager::getCharacterSheet(int64_t id) {
  using nlohmann::json;

  DbCursor res = _table.query(DbCondition(COL_ID, DbCondition::Type::EQ, id));
  if (!res.done()) {
    json raw = json::parse(res.col(2).asText());
    return characterSheetFromJSON(raw);
  } else {
    return std::optional<CharacterSheet>();
  }
}

void CharacterManager::saveCharacterSheet(int64_t id, int64_t user_id,
                                          const CharacterSheet &sheet) {
  std::string data = characterSheetToJSON(sheet).dump();
  _table.update(
      {DbColumnUpdate{COL_DATA, data}, DbColumnUpdate{COL_USER_ID, user_id}},
      DbCondition(COL_ID, DbCondition::Type::EQ, id));
}

int64_t CharacterManager::createCharacterSheet(int64_t user_id,
                                               const std::string &type) {
  for (const CharacterSheet &c : _character_sheet_templates) {
    if (c.character_sheet_type == type) {
      nlohmann::json raw = characterSheetToJSON(c);
      return _table.insert({DbColumnUpdate{COL_USER_ID, user_id},
                            DbColumnUpdate{COL_DATA, raw.dump()}});
    }
  }
  return -1;
}

// Attribute
// =============================================================================

CharacterManager::Attribute::Attribute(const std::string &name)
    : name_(name), type_(CharacterManager::AttributeType::LABEL) {}

CharacterManager::Attribute::Attribute(const std::string &name,
                                       const std::string &string)
    : name_(name),
      type_(CharacterManager::AttributeType::STRING),
      string_(string) {}

CharacterManager::Attribute::Attribute(const std::string &name, double number)
    : name_(name),
      type_(CharacterManager::AttributeType::NUMBER),
      number_(number) {}

CharacterManager::Attribute::Attribute(const std::string &name,
                                       const std::vector<Attribute> list)
    : name_(name), type_(CharacterManager::AttributeType::LIST) {
  // Initialze the attribute list
  new (&list_) AttributeList<Attribute>();

  list_.types = list;
}

CharacterManager::Attribute::Attribute(
    const std::string &name, const std::vector<Attribute> types,
    const std::vector<std::vector<Attribute>> &values)
    : Attribute(name, types) {
  list_.values = values;
}

CharacterManager::Attribute::Attribute(const Attribute &other)
    : Attribute(other.name_) {
  name_ = other.name_;
  type_ = other.type_;
  switch (type_) {
    case AttributeType::LABEL:
      break;
    case AttributeType::STRING:
      new (&string_) std::string(other.string_);
      break;
    case AttributeType::NUMBER:
      number_ = other.number_;
      break;
    case AttributeType::LIST:
      new (&list_) AttributeList<Attribute>(other.list_);
      break;
  }
}

CharacterManager::Attribute::Attribute(Attribute &&other)
    : Attribute(other.name_) {
  swap(*this, other);
}

CharacterManager::Attribute::~Attribute() {}

CharacterManager::Attribute &CharacterManager::Attribute::operator=(
    Attribute other) {
  swap(*this, other);
  return *this;
}

const std::string &CharacterManager::Attribute::name() const { return name_; }

CharacterManager::AttributeType CharacterManager::Attribute::type() const {
  return type_;
}

const std::string &CharacterManager::Attribute::string() const {
  return string_;
}

double CharacterManager::Attribute::number() const { return number_; }

const CharacterManager::AttributeList<CharacterManager::Attribute>
CharacterManager::Attribute::list() const {
  return list_;
}

void swap(CharacterManager::Attribute &attr1,
          CharacterManager::Attribute &attr2) {
  using std::swap;
  if (attr1.type_ == attr2.type_) {
    switch (attr1.type_) {
      case CharacterManager::AttributeType::LABEL:
        break;
      case CharacterManager::AttributeType::NUMBER:
        swap(attr1.number_, attr2.number_);
        break;
      case CharacterManager::AttributeType::STRING:
        swap(attr1.string_, attr2.string_);
        break;
      case CharacterManager::AttributeType::LIST:
        swap(attr1.list_.types, attr2.list_.types);
        swap(attr1.list_.values, attr2.list_.values);
        break;
    }
  } else {
    switch (attr1.type_) {
      case CharacterManager::AttributeType::LABEL:
        switch (attr2.type_) {
          case CharacterManager::AttributeType::LABEL:
            break;
          case CharacterManager::AttributeType::NUMBER:
            attr1.number_ = attr2.number_;
            break;
          case CharacterManager::AttributeType::STRING:
            attr1.string_ = attr2.string_;
            attr2.string_.std::string::~basic_string();
            break;
          case CharacterManager::AttributeType::LIST:
            attr1.list_ = attr2.list_;
            attr2.list_.AttributeList::~AttributeList();
            break;
        }
        break;
      case CharacterManager::AttributeType::NUMBER:
        switch (attr2.type_) {
          case CharacterManager::AttributeType::LABEL:
            attr2.number_ = attr1.number_;
            break;
          case CharacterManager::AttributeType::NUMBER:
            break;
          case CharacterManager::AttributeType::STRING: {
            double tmp = attr1.number_;
            attr1.string_ = attr2.string_;
            attr2.string_.std::string::~basic_string();
            attr2.number_ = tmp;
          } break;
          case CharacterManager::AttributeType::LIST: {
            double tmp = attr1.number_;
            attr1.list_ = attr2.list_;
            attr2.list_.AttributeList::~AttributeList();
            attr2.number_ = tmp;
          } break;
        }
        break;
      case CharacterManager::AttributeType::STRING:
        switch (attr2.type_) {
          case CharacterManager::AttributeType::LABEL:
            attr2.string_ = attr1.string_;
            attr1.string_.std::string::~basic_string();
            break;
          case CharacterManager::AttributeType::NUMBER: {
            double tmp = attr2.number_;
            attr2.string_ = attr1.string_;
            attr1.string_.std::string::~basic_string();
            attr1.number_ = tmp;
          } break;
          case CharacterManager::AttributeType::STRING:
            break;
          case CharacterManager::AttributeType::LIST: {
            std::string tmp = attr1.string_;
            attr1.list_ = attr2.list_;
            attr2.list_.AttributeList::~AttributeList();
            attr2.string_ = tmp;
          } break;
        }
        break;
      case CharacterManager::AttributeType::LIST:
        switch (attr2.type_) {
          case CharacterManager::AttributeType::LABEL:
            attr2.list_ = attr1.list_;
            attr1.list_.AttributeList::~AttributeList();
            break;
          case CharacterManager::AttributeType::NUMBER: {
            double tmp = attr2.number_;
            attr2.list_ = attr1.list_;
            attr1.list_.AttributeList::~AttributeList();
            attr1.number_ = tmp;
          } break;
          case CharacterManager::AttributeType::STRING: {
            std::string tmp = attr2.string_;
            attr2.list_ = attr1.list_;
            attr1.list_.AttributeList::~AttributeList();
            attr1.string_ = tmp;
          } break;
          case CharacterManager::AttributeType::LIST:
            break;
        }
        break;
    }
    swap(attr1.type_, attr2.type_);
  }
}
}  // namespace penandpaper
