#pragma once

#include <unordered_map>
#include <vector>

#include "HttpServer.h"
#include "Plugin.h"

class PluginManager : public HttpServer::RequestHandler {
 public:
  PluginManager();

  /** @brief Returns true if a plugin wishes to handle the given command. */
  bool hasCommand(const std::string &cmd);

  /** @brief Passes the command handling on to the correct plugin. */
  std::pair<WebSocketServer::ResponseType, std::string> handleCommand(
      const std::vector<std::string> &args);

  /** @brief Returns true if a plugin wishes to handle the given packet. */
  bool hasPacketHandler(const std::string &cmd) const;

  /** @brief Passes the packet handling on to the correct plugin. */
  std::pair<WebSocketServer::ResponseType, std::string> handlePacket(
      const std::string &name, const nlohmann::json &packet);

  /** @brief Returns a list of all plugins unique names. */
  std::vector<std::string> pluginNames() const;

  /** @brief handles the clients requests for a plugins client code. */
  void onRequest(const httplib::Request &req, httplib::Response &resp) override;

 private:
  /**
   * @brief Scans the plugins folder for plugins and loads and initialzies them.
   **/
  void loadPlugins();

  std::vector<Plugin> _plugins;

  /** @brief Maps command names to plugin indices */
  std::unordered_map<std::string, size_t> _commands;

  /** @brief Maps packet names to plugin indices */
  std::unordered_map<std::string, size_t> _packet_handlers;
};
