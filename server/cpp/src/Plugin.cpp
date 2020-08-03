#include "Plugin.h"

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

std::vector<std::string> Plugin::commands() const {
  std::vector<std::string> command_names;
  command_names.reserve(_commands.size());
  for (size_t i = 0; i < _commands.size(); ++i) {
    command_names.push_back(_commands[i].name);
  }
  return command_names;
}

void Plugin::load(const std::string &path) {
  std::string srcfile = path + "/plugin.lua";
  _script = std::move(LuaScript(srcfile));

  _script.loadStandardLibs();

  LuaScript::Variant lua_name = _script.global("PLUGIN_NAME");
  if (lua_name.type() == LuaScript::Type::STRING) {
    _name = lua_name.string();
  } else {
    _name = path;
  }

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
          _script.error("addCommand: Expected exactly two arguments.");
        }
        Command cmd;
        cmd.name = args[0].string();
        cmd.handler = args[1].string();
        _commands.push_back(cmd);
        return LuaScript::Variant();
      },
      {LuaScript::Type::STRING, LuaScript::Type::STRING});

  // Tell the script to initialize
  _script.call("init");
}
