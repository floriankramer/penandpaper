#include "CharacterManager.h"

CharacterManager::CharacterManager() {}

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
  if (args.size() != 1) {
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

  for (auto block_it : args[1].table()) {
    Variant::VariantPtr src_block = block_it.second;
    Variant::Table &block_table = src_block->asTable();

    AttributeBlock block;
    block.heading = block_it.first->asString();
    if (block_table.find(std::make_shared<Variant>("width")) !=
        block_table.end()) {
      block.width = block_table.find(std::make_shared<Variant>("width"))
                        ->second->asNumber();
    }

    std::shared_ptr<Variant> attributes =
        block_table.find(std::make_shared<Variant>("attributes"))->second;
    for (auto &attribute_it : attributes->asTable()) {
      Attribute attr;
      // TODO set the attribute name and value
    }

    // TODO: iterate the actions

    sheet.blocks.push_back(block);
  }

  _character_sheet_templates.push_back(sheet);
}

// Attribute
// =============================================================================

CharacterManager::Attribute::Attribute()
    : type_(CharacterManager::AttributeType::LABEL) {}

CharacterManager::Attribute::Attribute(const std::string &string)
    : type_(CharacterManager::AttributeType::STRING), string_(string) {}

CharacterManager::Attribute::Attribute(double number)
    : type_(CharacterManager::AttributeType::NUMBER), number_(number) {}

CharacterManager::Attribute::Attribute(const std::vector<Attribute> list)
    : type_(CharacterManager::AttributeType::LIST) {
  // Initialze the attribute list
  new (&list_) AttributeList<Attribute>();

  list_.types = list;
}

CharacterManager::Attribute::Attribute(const Attribute &other) : Attribute() {
  *this = other;
}

CharacterManager::Attribute::Attribute(Attribute &&other) : Attribute() {
  swap(*this, other);
}

CharacterManager::Attribute::~Attribute() {}

CharacterManager::Attribute &CharacterManager::Attribute::operator=(
    Attribute other) {
  swap(*this, other);
  return *this;
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
