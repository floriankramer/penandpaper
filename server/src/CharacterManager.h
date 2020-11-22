#pragma once

#include <optional>
#include <unordered_map>

#include "Database.h"
#include "HttpServer.h"
#include "LuaScript.h"
#include "PluginManager.h"

namespace penandpaper {

/**
 * @brief This class manages character sheet templates and instances. It loads
 * the templates from plugins and provides a rest api for instancing and
 * managing them.
 */
class CharacterManager : public HttpServer::RequestHandler {
  static const std::string COL_ID;
  static const std::string COL_USER_ID;
  static const std::string COL_DATA;

 public:
  /**
   * @brief The types of data a CharacterSheet can hold
   */
  enum class AttributeType {
    // The attribute has no value
    LABEL,
    NUMBER,
    STRING,
    // A list of structs. The structs are described by a list of non list
    // attributes.
    LIST
  };

  /**
   * @brief Stores data for list type attributes
   */
  template <typename T>
  struct AttributeList {
    std::vector<T> types;
    std::vector<std::vector<T>> values;
  };

  /**
   * @brief The Attribute struct is the basic data container used for storing
   * data in character sheets. It is a variant type.
   */
  class Attribute {
   public:
    Attribute(const std::string &name);
    Attribute(const std::string &name, const std::string &string);
    Attribute(const std::string &name, double number);
    Attribute(const std::string &name, const std::vector<Attribute> list);
    Attribute(const std::string &name, const std::vector<Attribute> types,
              const std::vector<std::vector<Attribute>> &values);

    Attribute(const Attribute &other);
    Attribute(Attribute &&other);

    virtual ~Attribute();

    Attribute &operator=(Attribute other);

    friend void swap(Attribute &attr1, Attribute &attr2);

    const std::string &name() const;
    AttributeType type() const;
    const std::string &string() const;
    double number() const;
    const AttributeList<Attribute> list() const;

   private:
    std::string name_;
    AttributeType type_;

    union {
      std::string string_;
      double number_;
      // The Attributes that define the columns of the table defined by a list
      // attribute.
      AttributeList<Attribute> list_;
    };
  };

  /**
   * @brief Character sheets are composed of blocks. Every block contains
   * attributes and actions.
   */
  struct AttributeBlock {
    std::string heading;
    int width = 4;

    std::vector<Attribute> attributes;
    std::unordered_map<std::string, std::string> actions;
  };

  /**
   * @brief A Character Sheet is a collection of blocks with some metadata.
   */
  struct CharacterSheet {
    std::string character_sheet_type;
    std::string character_name;
    /**
     * @brief The plugin which defined this character sheet
     */
    std::shared_ptr<Plugin> plugin;
    std::vector<AttributeBlock> blocks;
    int64_t id;
  };

  CharacterManager(Database *db);
  virtual ~CharacterManager();

  void registerPluginFunctions(std::shared_ptr<PluginManager> plugin_manager);

  virtual HttpServer::HttpResponse onRequest(
      UserManager::UserPtr user, const HttpServer::HttpRequest &req) override;

 private:
  void addCharacterSheetFromLua(LuaScript *script,
                                const std::vector<LuaScript::Variant> &args);

  nlohmann::json characterSheetToJSON(const CharacterSheet &sheet);
  nlohmann::json attributeToJSON(const Attribute &attr);

  Attribute attributeFromJSON(const nlohmann::json &j);
  CharacterSheet characterSheetFromJSON(const nlohmann::json &j);

  void openDatabaseTable();

  std::vector<CharacterSheet> listUserCharacterSheets(int64_t user_id);
  std::optional<CharacterSheet> getCharacterSheet(int64_t id);
  void saveCharacterSheet(int64_t id, int64_t user_id,
                          const CharacterSheet &sheet);
  /**
   * @brief Returns the new character sheets id on success, -1 on failure
   */
  int64_t createCharacterSheet(int64_t user_id, const std::string &type);

  std::vector<CharacterSheet> _character_sheet_templates;

  Database *_db;
  DbTable _table;
};
}  // namespace penandpaper
