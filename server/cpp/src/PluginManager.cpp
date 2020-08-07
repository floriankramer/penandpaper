#include "PluginManager.h"

#include "File.h"
#include "Logger.h"
#include "Util.h"

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
  for (const Plugin &p : _plugins) {
    names.push_back(p.name());
  }
  return names;
}

void PluginManager::onRequest(const httplib::Request &req,
                              httplib::Response &resp) {
  std::vector<std::string> parts = util::splitString(req.path, '/');
  // All requests need to be of the form '/plugin/<name>/file
  // or '/plugin/<name>/data/file'
  LOG_DEBUG << "PluginManager::onRequest : got a request for " << req.path
            << LOG_END;
  if (parts.size() != 3 && parts.size() != 4) {
    LOG_WARN << "PluginManager::onRequest : Rejected a malformed plugin "
                "request for path "
             << req.path << LOG_END;
    resp.body = "404 Not Found";
    resp.status = 404;
    return;
  }

  resp.body = "404 Not Found";
  resp.status = 404;

  std::string name = parts[1];
  for (const Plugin &p : _plugins) {
    if (p.name() == name) {
      LOG_DEBUG
          << "PluginManager::onRequest : Found a plugin to handle the request: "
          << p.name() << LOG_END;
      if (parts[2] == "html") {
        resp.status = 200;
        resp.set_header("Content-Type", "text/html");
        resp.body = p.html();
      } else if (parts[2] == "css") {
        resp.status = 200;
        resp.set_header("Content-Type", "text/css");
        resp.body = p.css();
      } else if (parts[2] == "js") {
        resp.status = 200;
        resp.set_header("Content-Type", "text/javascript");
        resp.body = p.js();
      } else if (parts[2] == "data") {
        resp.status = 200;
        std::string filename = parts[3];
        std::vector<char> data = p.data(filename);
        // We need to guess the mime type to allow for proper loading of images.
        std::string mime_type =
            HttpServer::guessMimeType(filename, "application/octet-stream");
        resp.set_content(data.data(), data.size(), mime_type.c_str());
      } else {
        LOG_WARN << "PluginManager::onRequest : Got a request for " << parts[2]
                 << " in plugin " << name << " but the datatype is not known."
                 << LOG_END;
      }
      return;
    }
  }
}
