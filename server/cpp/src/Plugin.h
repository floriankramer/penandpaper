#pragma once

#include <string>
#include <vector>

#include "LuaScript.h"

/**
 * @brief A lua plugin that canbe used to add ruleset specific functionality
 */
class Plugin {
  struct Command {
    std::string name;
    std::string handler;
  };

 public:
  Plugin();

  /**
   * @brief Load the plugin from a folder at path
   */
  Plugin(const std::string &path);

  /**
   * @brief Passes the command handling on to the plugin.
   */
  std::string onCommand(const std::vector<std::string> &parts);

  /**
   * @brief Returns a list of commands this plugin would like to handle
   */
  std::vector<std::string> commands() const;

 private:
  /** @brief Load the plugin from a folder at path */
  void load(const std::string &path);

  /** @brief The lua script that handles server sided logic */
  LuaScript _script;

  /** @brief A list of chat commands this plugin handles */
  std::vector<Command> _commands;

  /** @brief A human readable name */
  std::string _name;
};
