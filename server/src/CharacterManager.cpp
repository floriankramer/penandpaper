#include "CharacterManager.h"

#include "Logger.h"
#include "Util.h"

namespace penandpaper {

CharacterManager::CharacterManager() {}

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

  for (auto block_it = blocks_table.stringBegin();
       block_it != blocks_table.stringEnd(); ++block_it) {
    Variant::VariantPtr src_block = block_it->second;
    LuaScript::Table &block_table = src_block->asTable();

    AttributeBlock block;
    block.heading = block_it->first;
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
      attr_j["type"] = "label";
      attr_j["value"] = attr.number();
      break;
    case AttributeType::LIST: {
      attr_j["type"] = "list";
      json types_list_j = json::array();
      for (const Attribute &type : attr.list().types) {
        types_list_j.push_back(attributeToJSON(type));
      }
      attr_j["types"] = types_list_j;
      // TODO: encode the values
    } break;
    case AttributeType::STRING:
      attr_j["type"] = "string";
      attr_j["value"] = attr.string();
      break;
  }
  return attr_j;
}

HttpServer::HttpResponse CharacterManager::onRequest(
    const HttpServer::HttpRequest &req) {
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
        } else {
          LOG_DEBUG << "CharacterManager::onRequest: '"
                    << c.character_sheet_type << "' == '" << parts[2]
                    << "': " << (c.character_sheet_type == parts[2]) << LOG_END;
          LOG_DEBUG << "CharacterManager::onRequest: '"
                    << c.character_sheet_type.size() << "' == '"
                    << parts[2].size() << "': "
                    << (c.character_sheet_type.size() == parts[2].size())
                    << LOG_END;
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
