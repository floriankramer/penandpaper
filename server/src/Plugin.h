#pragma once

#include <nlohmann/json.hpp>
#include <string>
#include <utility>
#include <vector>
#include <functional>

#include "LuaScript.h"

// TODO: Think about this from an architecture point of view
#include "WebSocketServer.h"

/**
 * @brief A lua plugin that canbe used to add ruleset specific functionality
 */
class Plugin {
  struct Command {
    std::string name;
    std::string handler;
  };

  /**
   * @brief Contains all data required to inform the simulation about packets
   *        we want to handle.
   */
  struct PacketTrap {
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
  std::pair<WebSocketServer::ResponseType, std::string> onCommand(
      const std::vector<std::string> &parts);

  /**
   * @brief Passes the packet handling on to the plugin.
   */
  std::pair<WebSocketServer::ResponseType, std::string> onPacket(
      const std::string &name,
      const nlohmann::json &packet);

  /**
   * @brief Returns a list of commands this plugin would like to handle
   */
  std::vector<std::string> commands() const;

  /**
   * @brief Returns a list of packets this plugin would like to handle
   */
  std::vector<std::string> packets() const;

  /** @brief The plugin name. A string of [a-zA-Z_]. */
  const std::string &name() const;

  /** @brief Returns the plugins client html code or the empty string.*/
  const std::string &html() const;

  /** @brief Returns the plugins client css code or the empty string.*/
  const std::string &css() const;

  /** @brief Returns the plugins client js code or the empty string.*/
  const std::string &js() const;

  /** @brief Loads the given file from the plugins data folder. */
  const std::vector<char> data(const std::string &filename) const;

  /**
   * @brief f should be a function that takes a string and sends it to all
   * players chats.
   */
  void setWriteToChat(std::function<void(const std::string&)> f);

  /**
   * @brief f should be a function that takes a string and sends it to all
   * players.
   */
  void setBroadcastPacket(std::function<void(const std::string&)> f);

 private:
  /** @brief Load the plugin from a folder at path */
  void load(const std::string &path);

  /** @brief Replaces anything not in [a-zA-Z_] by _*/
  std::string cleanName(const std::string &name) const;

  /** @brief The lua script that handles server sided logic */
  LuaScript _script;

  /** @brief A list of chat commands this plugin handles */
  std::vector<Command> _commands;

  /** @brief A list of packets this plugin handles */
  std::vector<PacketTrap> _packets;

  /** @brief A human readable name */
  std::string _name;

  /** @brief The plugins location on the filesystem. */
  std::string _path;

  std::string _html;
  std::string _css;
  std::string _js;

  /** @brief A function that can be called to write data to the chat. */
  std::function<void(const std::string &)> _writeToChat;


  /** @brief A function that can be called to write data to the chat. */
  std::function<void(const std::string &)> _broadcast_packet;
};
