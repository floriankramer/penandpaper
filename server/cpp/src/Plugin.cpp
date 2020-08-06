#include "Plugin.h"

#include <fstream>

#include "Logger.h"

Plugin::Plugin() {}

Plugin::Plugin(const std::string &path) { load(path); }

std::pair<WebSocketServer::ResponseType, std::string> Plugin::onCommand(
    const std::vector<std::string> &parts) {
  std::string cmd = parts[0].substr(1);
  for (size_t i = 0; i < _commands.size(); ++i) {
    if (_commands[i].name == cmd) {
      std::vector<LuaScript::Variant> args;
      args.reserve(parts.size());
      for (const std::string &s : parts) {
        args.push_back(s);
      }

      std::vector<LuaScript::Variant> res =
          _script.callVarArg(_commands[i].handler, 2, args);
      if (res[0].type() == LuaScript::Type::NUMBER &&
          res[1].type() == LuaScript::Type::STRING) {
        return {WebSocketServer::ResponseType(res[0].number()),
                res[1].string()};
      } else {
        LOG_ERROR << "The command function handler for " << cmd << " : "
                  << _commands[i].handler
                  << " does not return a number and a string." << LOG_END;
        LOG_ERROR << "Expected: (number, string), got (" << res[0].typeName()
                  << ", " << res[1].typeName() << ")" << LOG_END;
        return {WebSocketServer::ResponseType::RETURN, "Internal error"};
      }
    }
  }
  return {WebSocketServer::ResponseType::RETURN,
          "The plugin " + _name + " doesn't implement command " + cmd};
}

std::pair<WebSocketServer::ResponseType, std::string> Plugin::onPacket(
    const std::string &name, const nlohmann::json &packet) {
  for (size_t i = 0; i < _packets.size(); ++i) {
    if (_packets[i].name == name) {
      // Transform the json into a lua table
      LuaScript::Variant arg(packet);

      std::vector<LuaScript::Variant> res =
          _script.call(_commands[i].handler, 2, {arg});
      if (res[0].type() == LuaScript::Type::NUMBER &&
          res[1].type() == LuaScript::Type::TABLE) {
        return {WebSocketServer::ResponseType(res[0].number()),
                res[1].toJson()};
      } else {
        LOG_ERROR << "The packet handler for " << name << " : "
                  << _packets[i].handler
                  << " does not return a number and a string." << LOG_END;
        LOG_ERROR << "Expected: (number, string), got (" << res[0].typeName()
                  << ", " << res[1].typeName() << ")" << LOG_END;
        return {WebSocketServer::ResponseType::RETURN, "Internal error"};
      }
    }
  }
  return {WebSocketServer::ResponseType::RETURN,
          "The plugin " + _name + " doesn't implement a packet handler for " +
              name};
}

std::vector<std::string> Plugin::commands() const {
  std::vector<std::string> command_names;
  command_names.reserve(_commands.size());
  for (size_t i = 0; i < _commands.size(); ++i) {
    command_names.push_back(_commands[i].name);
  }
  return command_names;
}

std::vector<std::string> Plugin::packets() const {
  std::vector<std::string> packet_names;
  packet_names.reserve(_packets.size());
  for (size_t i = 0; i < _packets.size(); ++i) {
    packet_names.push_back(_packets[i].name);
  }
  return packet_names;
}

const std::string &Plugin::name() const { return _name; }

const std::string &Plugin::html() const { return _html; }

const std::string &Plugin::css() const { return _css; }

const std::string &Plugin::js() const { return _js; }

const std::vector<char> Plugin::data(const std::string &filename) const {
  std::ifstream css_in(_path + "/data/" + filename);
  std::vector<char> buffer;
  if (css_in.is_open()) {
    // determine the file size
    css_in.seekg(0, std::ios::end);
    size_t size = css_in.tellg();
    // allocate space for the data
    buffer.resize(size);
    // read the entire file
    css_in.read(buffer.data(), size);
  }
  return buffer;
}

void Plugin::load(const std::string &path) {
  _path = path;
  // Load the server sided lua plugin
  std::string srcfile = path + "/plugin.lua";
  _script = std::move(LuaScript(srcfile));

  _script.loadStandardLibs();

  LuaScript::Variant lua_name = _script.global("PLUGIN_NAME");
  if (lua_name.type() == LuaScript::Type::STRING) {
    _name = lua_name.string();
  } else {
    size_t idx = path.rfind('/');
    if (idx != std::string::npos) {
      _name = path.substr(idx);
    } else {
      _name = path;
    }
  }
  // remove forbidden characters from the name
  _name = cleanName(_name);

  // The LUA_PATH global is used to find files loaded via require.
  // Each ? is replaced by the required name.
  _script.setGlobal("LUA_PATH", "?;?.lua");

  _script.setGlobal("SEND_TO_ALL",
                    double(WebSocketServer::ResponseType::BROADCAST));
  _script.setGlobal("SEND_TO_SENDER",
                    double(WebSocketServer::ResponseType::RETURN));
  _script.setGlobal("SEND_TO_NOBODY",
                    double(WebSocketServer::ResponseType::SILENCE));

  //  _script.registerFunction("print",
  //                           [this](std::vector<LuaScript::Variant> args) {
  //                             LOG_INFO << "Plugin " << _name << ": "
  //                                      << args[0].string() << LOG_END;
  //                             return LuaScript::Variant();
  //                           },
  //                           {LuaScript::Type::STRING});

  _script.registerFunction(
      "addCommand",
      [this](std::vector<LuaScript::Variant> args) {
        if (args.size() != 2) {
          _script.error("addCommand: Expected exactly two arguments but got " +
                        std::to_string(args.size()));
        }
        Command cmd;
        cmd.name = args[0].string();
        cmd.handler = args[1].string();
        _commands.push_back(cmd);
        return LuaScript::Variant();
      },
      {LuaScript::Type::STRING, LuaScript::Type::STRING});

  _script.registerFunction(
      "listenToPacket",
      [this](std::vector<LuaScript::Variant> args) {
        if (args.size() != 2) {
          _script.error(
              "listenToPacket: Expected exactly two arguments but got " +
              std::to_string(args.size()));
        }
        PacketTrap pt;
        // prefix the requested name with the plugin name
        pt.name = _name + "::" + args[0].string();
        pt.handler = args[1].string();
        _packets.push_back(pt);
        return LuaScript::Variant();
      },
      {LuaScript::Type::STRING, LuaScript::Type::STRING});

  // Tell the script to initialize
  _script.call("init");

  // Load the client sided plugin data
  {
    std::ifstream html_in(path + "/plugin.html");
    if (html_in.is_open()) {
      // determine the file size
      html_in.seekg(0, std::ios::end);
      size_t size = html_in.tellg();
      // allocate space for the data
      std::vector<char> buffer;
      buffer.resize(size + 1);
      // read the entire file
      html_in.read(buffer.data(), size);
      // add a delimiting 0
      buffer.back() = 0;
      _html = buffer.data();
    }
  }
  {
    std::ifstream css_in(path + "/plugin.css");
    if (css_in.is_open()) {
      // determine the file size
      css_in.seekg(0, std::ios::end);
      size_t size = css_in.tellg();
      // allocate space for the data
      std::vector<char> buffer;
      buffer.resize(size + 1);
      // read the entire file
      css_in.read(buffer.data(), size);
      // add a delimiting 0
      buffer.back() = 0;
      _html = buffer.data();
    }
  }
  {
    std::ifstream js_in(path + "/plugin.js");
    if (js_in.is_open()) {
      // determine the file size
      js_in.seekg(0, std::ios::end);
      size_t size = js_in.tellg();
      // allocate space for the data
      std::vector<char> buffer;
      buffer.resize(size + 1);
      // read the entire file
      js_in.read(buffer.data(), size);
      // add a delimiting 0
      buffer.back() = 0;
      _html = buffer.data();
    }
  }
}

std::string Plugin::cleanName(const std::string &name) const {
  std::vector<char> buf;
  buf.reserve(name.size() + 1);
  for (size_t i = 0; i < name.size(); ++i) {
    if (std::isalpha(name[i]) || name[i] == '_') {
      buf.push_back(name[i]);
    } else {
      LOG_WARN << "Character '" << name[i]
               << "' is not allowed in a plugin name" << LOG_END;
      buf.push_back('_');
    }
  }
  buf.push_back(0);
  return std::string(buf.data());
}
