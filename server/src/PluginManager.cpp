#include "PluginManager.h"

#include "Logger.h"
#include "Util.h"
#include "utils/File.h"

PluginManager::PluginManager() {}

void PluginManager::loadPlugins() {
  LOG_INFO << "Loading plugins from './plugins'..." << LOG_END;
  std::vector<std::string> names = File::listDir("plugins");
  LOG_INFO << "Found " << names.size() << " plugins" << LOG_END;
  for (const std::string &name : names) {
    LOG_INFO << "Loading plugin at " << name << LOG_END;
    std::shared_ptr<Plugin> plugin = std::make_shared<Plugin>();
    for (const Plugin::AdditionalApiFunction &apif :
         _additional_api_functions) {
      plugin->addApiFunction(apif.function_name, apif.function,
                             apif.argument_types);
    }
    plugin->load("plugins/" + name);
    _plugins.push_back(plugin);

    for (const std::string &cmd : _plugins.back()->commands()) {
      _commands[cmd] = _plugins.size() - 1;
    }

    for (const std::string &cmd : _plugins.back()->packets()) {
      _packet_handlers[cmd] = _plugins.size() - 1;
    }
  }
}

void PluginManager::addApiFunction(
    const std::string &function_name, LuaScript::ApiFunction function,
    const std::vector<LuaScript::Type> &argument_types) {
  _additional_api_functions.push_back(
      {function_name, function, argument_types});
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
  return _plugins[it->second]->onCommand(args);
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
  return _plugins[it->second]->onPacket(name, packet);
}

std::vector<std::string> PluginManager::pluginNames() const {
  std::vector<std::string> names;
  names.reserve(_plugins.size());
  for (const std::shared_ptr<Plugin> &p : _plugins) {
    names.push_back(p->name());
  }
  return names;
}

HttpServer::HttpResponse PluginManager::onRequest(
    const HttpServer::HttpRequest &req) {
  HttpServer::HttpResponse resp;

  std::vector<std::string> parts = util::splitString(req.path, '/');
  // All requests need to be of the form '/plugin/<name>/file
  // or '/plugin/<name>/data/file'
  LOG_DEBUG << "PluginManager::onRequest : got a request for " << req.path
            << LOG_END;
  if (parts.size() != 3 && parts.size() != 4) {
    LOG_WARN << "PluginManager::onRequest : Rejected a malformed plugin "
                "request for path "
             << req.path << LOG_END;
    resp.setError(404, "Not Found");
    return resp;
  }

  resp.setError(404, "Not Found");

  std::string name = parts[1];
  for (const std::shared_ptr<Plugin> &p : _plugins) {
    if (p->name() == name) {
      if (parts[2] == "html") {
        resp.status_code = 200;
        resp.setMimeType("text/html");
        resp.setBody(p->html());
      } else if (parts[2] == "css") {
        resp.status_code = 200;
        resp.setMimeType("text/css");
        resp.setBody(p->css());
      } else if (parts[2] == "js") {
        resp.status_code = 200;
        resp.setMimeType("text/javascript");
        resp.setBody(p->js());
      } else if (parts[2] == "data") {
        resp.status_code = 200;
        std::string filename = parts[3];
        std::vector<char> data = p->data(filename);
        // We need to guess the mime type to allow for proper loading of images.
        std::string mime_type =
            HttpServer::guessMimeType(filename, "application/octet-stream");
        resp.setMimeType(mime_type);
        resp.body = std::move(data);
      } else {
        LOG_WARN << "PluginManager::onRequest : Got a request for " << parts[2]
                 << " in plugin " << name << " but the datatype is not known."
                 << LOG_END;
      }
      break;
    }
  }
  return resp;
}

void PluginManager::setWriteToChat(std::function<void(const std::string &)> f) {
  for (std::shared_ptr<Plugin> &p : _plugins) {
    p->setWriteToChat(f);
  }
}

void PluginManager::setBroadcastPacket(
    std::function<void(const std::string &)> f) {
  for (std::shared_ptr<Plugin> &p : _plugins) {
    p->setBroadcastPacket(f);
  }
}
