#pragma once

#include <unordered_map>
#include <vector>

#include "Plugin.h"

class PluginManager {
 public:
  PluginManager();

  bool hasCommand(const std::string &cmd);
  std::pair<WebSocketServer::ResponseType, std::string> handleCommand(
      const std::vector<std::string> &args);


  bool hasPacketHandler(const std::string &cmd);
  std::pair<WebSocketServer::ResponseType, std::string> handlePacket(
      const std::string &name,
      const nlohmann::json &packet);

 private:
  void loadPlugins();

  std::vector<Plugin> _plugins;

  /** @brief Maps command names to plugin indices */
  std::unordered_map<std::string, size_t> _commands;


  /** @brief Maps packet names to plugin indices */
  std::unordered_map<std::string, size_t> _packet_handlers;
};
