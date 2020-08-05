#include "PluginManager.h"

#include "File.h"
#include "Logger.h"

PluginManager::PluginManager() { loadPlugins(); }

void PluginManager::loadPlugins() {
  LOG_INFO << "Loading plugins from './plugins'..." << LOG_END;
  std::vector<std::string> names = File::listDir("plugins");
  LOG_INFO << "Found " << names.size() << " plugins" << LOG_END;
  for (const std::string &name : names) {
    LOG_INFO << "Loading plugin at " << name << LOG_END;
    _plugins.emplace_back("plugins/" + name);

    for (const std::string &cmd : _plugins.back().commands()) {
      _commands[cmd] = _plugins.size() - 1;
    }

    for (const std::string &cmd : _plugins.back().packets()) {
      _packet_handlers[cmd] = _plugins.size() - 1;
    }
  }
}

bool PluginManager::hasCommand(const std::string &cmd) {
  return _commands.count(cmd) > 0;
}

std::pair<WebSocketServer::ResponseType, std::string>
PluginManager::handleCommand(const std::vector<std::string> &args) {
  std::string cmd = args[0].substr(1);
  auto it = _commands.find(cmd);
  if (it == _commands.end()) {
    throw std::runtime_error("PluginManager::handleCommand : command " + cmd +
                             " is not handled by any plugin.");
  }
  return _plugins[it->second].onCommand(args);
}

bool PluginManager::hasPacketHandler(const std::string &cmd) const {
  return _packet_handlers.count(cmd) > 0;
}

std::pair<WebSocketServer::ResponseType, std::string>
PluginManager::handlePacket(const std::string &name,
                            const nlohmann::json &packet) {
  auto it = _packet_handlers.find(name);
  if (it == _packet_handlers.end()) {
    throw std::runtime_error("PluginManager::handlePacket : packet " + name +
                             " is not handled by any plugin.");
  }
  return _plugins[it->second].onPacket(name, packet);
}


std::vector<std::string> PluginManager::pluginNames() const {
  std::vector<std::string> names;
  names.reserve(_plugins.size());
  for (const Plugin & p : _plugins) {
    names.push_back(p.name());
  }
  return names;
}
