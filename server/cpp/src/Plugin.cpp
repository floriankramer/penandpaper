#include "Plugin.h"

#include "Logger.h"

Plugin::Plugin() {}

Plugin::Plugin(const std::string &path) { load(path); }

std::string Plugin::onCommand(const std::vector<std::string> &parts) {
  std::string cmd = parts[0].substr(1);
  for (size_t i = 0; i < _commands.size(); ++i) {
    if (_commands[i].name == cmd) {
      std::vector<LuaScript::Variant> res =
          _script.call(_commands[i].handler, 1);
      if (res[0].type() == LuaScript::Type::STRING) {
        return res[0].string();
      } else {
        LOG_ERROR << "The command function handler for " << cmd << " : "
                  << _commands[i].handler << " does not return a string.";
        return "Internal error";
      }
    }
  }
  return "The plugin " + _name + " doesn't implement command " + cmd;
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
