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

 private:
  void loadPlugins();

  std::vector<Plugin> _plugins;

  /** @brief Maps command names to plugin indices */
  std::unordered_map<std::string, size_t> _commands;
};
