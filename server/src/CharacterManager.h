#pragma once

#include <unordered_map>

#include "HttpServer.h"
#include "LuaScript.h"
#include "PluginManager.h"

/**
 * @brief This class manages character sheet templates and instances. It loads
 * the templates from plugins and provides a rest api for instancing and
 * managing them.
 */
class CharacterManager : public HttpServer::RequestHandler {
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
    Attribute();
    Attribute(const std::string &string);
    Attribute(double number);
    Attribute(const std::vector<Attribute> list);

    Attribute(const Attribute &other);
    Attribute(Attribute &&other);

    virtual ~Attribute();

    Attribute &operator=(Attribute other);

    friend void swap(Attribute &attr1, Attribute &attr2);

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
  struct CharacterBlock {
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
    std::vector<CharacterBlock> blocks;
  };

  CharacterManager();

  void registerPluginFunctions(std::shared_ptr<PluginManager> plugin_manager);

  virtual HttpServer::HttpResponse onRequest(
      const HttpServer::HttpRequest &req);

 private:
  void addCharacterSheetFromLua(const std::vector<LuaScript::Variant> &args);

  std::vector<CharacterSheet> _character_sheet_templates;
};
