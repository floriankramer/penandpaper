#include "LuaScript.h"

#include <sstream>

extern "C" {
#include "lauxlib.h"
#include "lua.h"
#include "lualib.h"
}

#include "Logger.h"

const std::string LuaScript::REGISTRY_THIS = "cscript";
const char *LuaScript::TYPE_NAMES[8] = {"NIL",      "BOOLEAN",  "NUMBER",
                                        "STRING",   "FUNCTION", "CFUNCTION",
                                        "USERDATA", "TABLE"};

// This is the error handler that is called every time a lua_pcall function
// catches an error. It simply adds a stacktrace and converts the error to a
// string.
int luaOnError(lua_State *state) {
  // This error handler is based upon the one in lua.c
  const char *msg = lua_tostring(state, 1);
  if (msg == NULL) {
    luaL_traceback(state, state, "Error object is not a string", 1);
    // We can't convert the object to a string, simply reuse it
    return 1;
  } else {
    // generate a traceback that includes the msg
    luaL_traceback(state, state, msg, 1);
    return 1;
  }
}

int luaFunction(lua_State *state) {
  // Read the script pointer
  LuaScript *script = (LuaScript *)lua_touserdata(state, lua_upvalueindex(1));

  // read the handler index
  int handler = lua_tointeger(state, lua_upvalueindex(2));

  // turn the function arguments into variants
  std::vector<LuaScript::Variant> arguments;
  arguments.reserve(lua_gettop(state));
  for (int i = 1; i <= lua_gettop(state); ++i) {
    arguments.emplace_back(state, i);
  }

  LuaScript::Variant result = script->onFunctionCall(handler, arguments);
  result.push(state);
  return 1;
}

LuaScript::LuaScript() { _lua_state = luaL_newstate(); }

LuaScript::LuaScript(const std::string &path) : LuaScript() { load(path); }

LuaScript::LuaScript(LuaScript &&other) {
  _lua_state = other._lua_state;
  other._lua_state = nullptr;
}

LuaScript::~LuaScript() {
  if (_lua_state != nullptr) {
    lua_close(_lua_state);
  }
}

void LuaScript::operator=(LuaScript &&other) {
  lua_close(_lua_state);
  _lua_state = other._lua_state;
  other._lua_state = nullptr;
}

int LuaScript::stackSize() const { return lua_gettop(_lua_state); }

LuaScript::Variant LuaScript::onFunctionCall(int index,
                                             std::vector<Variant> arguments) {
  StackGuard stack_guard(this, "LuaScript::onFunctionCall");
  if (index >= 0 && size_t(index) < _function_handlers.size()) {
    LuaFunction &f = _function_handlers[index];
    for (size_t i = 0; i < std::min(arguments.size(), f.arguments.size());
         ++i) {
      if (arguments[i].type() != f.arguments[i]) {
        lua_checkstack(_lua_state, 1);
        {
          std::ostringstream s;
          s << f.name << " : The argument " << i << " is expected to have type "
            << TYPE_NAMES[int(f.arguments[i])] << " but has type "
            << TYPE_NAMES[int(arguments[i].type())];
          std::string ss = s.str();
          lua_pushstring(_lua_state, ss.c_str());
        }
        // throw an error and return
        lua_error(_lua_state);
      }
    }
    return f.handler(this, arguments);
  } else {
    LOG_WARN << "LuaScript::onFunctionCall : No handler registered for index "
             << index << " (" << _function_handlers.size()
             << " is the max index)." << LOG_END;
  }
  return Variant();
}

void LuaScript::registerFunction(const std::string name, ApiFunction handler,
                                 const std::vector<Type> &arguments) {
  StackGuard stack_guard(this, "LuaScript::registerFunction");
  // Store a reference to this script in the closure
  lua_pushlightuserdata(_lua_state, this);

  // Store the index of the handler in the closure
  lua_pushinteger(_lua_state, _function_handlers.size());
  _function_handlers.push_back({handler, arguments, name});

  // Add the closure and regitser it in the global namespace
  lua_pushcclosure(_lua_state, luaFunction, 2);
  lua_setglobal(_lua_state, name.c_str());
}

void LuaScript::loadStandardLibs() { luaL_openlibs(_lua_state); }

LuaScript::Variant LuaScript::global(const std::string &s) const {
  StackGuard stack_guard(this, "LuaScript::global");
  lua_checkstack(_lua_state, 1);
  lua_getglobal(_lua_state, s.c_str());
  Variant v(_lua_state, -1);
  lua_pop(_lua_state, 1);
  return v;
}

void LuaScript::setGlobal(const std::string &s, Variant value) {
  StackGuard stack_guard(this, "LuaScript::setGlobal");
  value.push(_lua_state);
  lua_setglobal(_lua_state, s.c_str());
}

std::vector<LuaScript::Variant> LuaScript::call(
    const std::string &function, const std::vector<Variant> args) {
  StackGuard stack_guard(this, "LuaScript::call");

  std::vector<LuaScript::Variant> ret;

  // load the error handler
  lua_pushcfunction(_lua_state, luaOnError);
  int error_handler = lua_gettop(_lua_state);

  // load the function
  lua_getglobal(_lua_state, function.c_str());

  // load the arguments
  for (const Variant &arg : args) {
    arg.push(_lua_state);
  }

  // now call the function
  int r = lua_pcall(_lua_state, args.size(), LUA_MULTRET, error_handler);
  if (r) {
    // print and pop the error
    LOG_ERROR << "LuaScript::call : " << function << " : "
              << lua_tostring(_lua_state, -1) << LOG_END;
    lua_pop(_lua_state, 1);
    return {};
  }
  int num_results = lua_gettop(_lua_state) - error_handler;
  for (int i = error_handler + 1; i < error_handler + 1 + num_results; ++i) {
    ret.emplace_back(_lua_state, i);
  }
  // pop the results
  lua_pop(_lua_state, num_results + 1);
  return ret;
}

std::vector<LuaScript::Variant> LuaScript::callVarArg(
    const std::string &function, const std::vector<Variant> &args) {
  StackGuard stack_guard(this, "LuaScript::callVarArg");
  std::vector<LuaScript::Variant> ret;

  // Load the error handler
  lua_pushcfunction(_lua_state, luaOnError);
  int error_handler = lua_gettop(_lua_state);

  // load the function
  lua_getglobal(_lua_state, function.c_str());

  // load the arguments
  lua_createtable(_lua_state, args.size(), 0);
  for (size_t i = 0; i < args.size(); ++i) {
    const Variant &arg = args[i];
    arg.push(_lua_state);
    lua_seti(_lua_state, -2, i + 1);
  }

  // now call the function
  int r = lua_pcall(_lua_state, 1, LUA_MULTRET, error_handler);
  if (r) {
    // print and pop the error
    LOG_ERROR << "LuaScript::callVarArg : " << function << " : "
              << lua_tostring(_lua_state, -1) << LOG_END;
    lua_pop(_lua_state, 1);
    return {};
  }
  int num_results = lua_gettop(_lua_state) - error_handler;
  for (int i = error_handler + 1; i < error_handler + 1 + num_results; ++i) {
    ret.emplace_back(_lua_state, i);
  }
  // pop the results
  lua_pop(_lua_state, num_results + 1);

  return ret;
}

void LuaScript::error(const std::string &text) {
  lua_pushstring(_lua_state, text.c_str());
  lua_error(_lua_state);
}

void LuaScript::load(const std::string &path) {
  lua_pushcfunction(_lua_state, luaOnError);
  int error_handler = lua_gettop(_lua_state);
  if (luaL_loadfile(_lua_state, path.c_str())) {
    throw std::runtime_error(
        "LuaScript::load : Unable to load the lua script at " + path + " : " +
        lua_tostring(_lua_state, -1));
  }
  if (lua_pcall(_lua_state, 0, 0, error_handler)) {
    throw std::runtime_error(
        "LuaScript::load : Unable to evaluate the lua script at " + path +
        " : " + lua_tostring(_lua_state, -1));
  }
  // pop the error handler
  lua_pop(_lua_state, 1);

  // Add a new entry to the registry
  lua_pushlightuserdata(_lua_state, this);
  lua_setfield(_lua_state, LUA_REGISTRYINDEX, REGISTRY_THIS.c_str());
}

// VARIANT
// =============================================================================

LuaScript::Variant::Variant() : _type(Type::NIL) {}

LuaScript::Variant::Variant(bool boolean)
    : _type(Type::BOOLEAN), _boolean(boolean) {}

LuaScript::Variant::Variant(double number)
    : _type(Type::NUMBER), _number(number) {}

LuaScript::Variant::Variant(const std::string &string)
    : _type(Type::STRING), _string(string) {}

LuaScript::Variant::Variant(void *userdata)
    : _type(Type::USERDATA), _userdata(userdata) {}

LuaScript::Variant::Variant(const Table table)
    : _type(Type::TABLE), _table(table) {}

LuaScript::Variant::Variant(lua_State *state, int idx) {
  int type = lua_type(state, idx);
  if (type == LUA_TNUMBER) {
    _type = Type::NUMBER;
    _number = lua_tonumber(state, idx);
  } else if (type == LUA_TNIL) {
    _type = Type::NIL;
  } else if (type == LUA_TBOOLEAN) {
    _type = Type::BOOLEAN;
    _boolean = lua_toboolean(state, idx);
  } else if (type == LUA_TSTRING) {
    _type = Type::STRING;
    new (&_string) std::string(lua_tostring(state, idx));
  } else if (type == LUA_TTABLE) {
    new (&_table) decltype(_table)();
    // ensure we have enough space for iteration
    lua_checkstack(state, 2);
    // add an empty key
    lua_pushnil(state);
    // iterate the table
    while (lua_next(state, idx) != 0) {
      int keytype = lua_type(state, -2);
      if (keytype == LUA_TSTRING) {
        VariantPtr key = std::make_shared<Variant>(state, -2);
        _table[key] = std::make_shared<Variant>(state, -1);
      } else {
        LOG_ERROR
            << "LuaScript::Variant::Variant : Unable to load a key of type "
            << lua_typename(state, -2) << " from a table." << LOG_END;
      }
      lua_pop(state, 1);
    }
    _type = Type::TABLE;
  } else if (type == LUA_TUSERDATA || type == LUA_TLIGHTUSERDATA) {
    _type = Type::USERDATA;
    _userdata = lua_touserdata(state, idx);
  } else {
    LOG_WARN << "Variant::Variant : unable to represent type "
             << lua_typename(state, idx) << LOG_END;
    _type = Type::NIL;
  }
}

LuaScript::Variant::Variant(const Variant &other) : _type(other._type) {
  switch (_type) {
    case Type::NIL:
      // do nothing
      break;
    case Type::BOOLEAN:
      _boolean = other._boolean;
      break;
    case Type::NUMBER:
      _number = other._number;
      break;
    case Type::STRING:
      // Use placement new to call the copy constructor.
      new (&_string) std::string(other._string);
      break;
    case Type::USERDATA:
      _userdata = other._userdata;
      break;
    case Type::TABLE:
      // Use placement new to call the copy constructor.
      new (&_table) decltype(_table)(other._table);
      break;
    default:
      throw std::runtime_error(
          "LuaScript::Variant::Variant : Unsupported variant type: " +
          std::to_string(int(_type)));
  }
}

LuaScript::Variant::Variant(Variant &&other) : _type(other._type) {
  switch (_type) {
    case Type::NIL:
      // do nothing
      break;
    case Type::BOOLEAN:
      _boolean = other._boolean;
      break;
    case Type::NUMBER:
      _number = other._number;
      break;
    case Type::STRING:
      // Use placement new to call the copy constructor.
      new (&_string) std::string(std::move(other._string));
      break;
    case Type::USERDATA:
      _userdata = other._userdata;
      break;
    case Type::TABLE:
      // Use placement new to call the copy constructor.
      new (&_table) decltype(_table)(std::move(other._table));
      break;
    default:
      throw std::runtime_error("Unsupported variant type: " +
                               std::to_string(int(_type)));
  }
}

LuaScript::Variant::Variant(const nlohmann::json &json) {
  if (json.is_number()) {
    _type = Type::NUMBER;
    _number = json.get<double>();
  } else if (json.is_string()) {
    _type = Type::STRING;
    new (&_string) std::string(json.get<std::string>());
  } else if (json.is_boolean()) {
    _type = Type::BOOLEAN;
    _boolean = json.get<bool>();
  } else if (json.is_null()) {
    _type = Type::NIL;
  } else if (json.is_object()) {
    _type = Type::TABLE;
    new (&_table) decltype(_table)();
    for (auto it : json.items()) {
      VariantPtr key = std::make_shared<Variant>(it.key());
      _table[key] = std::make_shared<Variant>(it.value());
    }
  } else {
    throw std::runtime_error(
        "LuaScript::Variant::Variant : unsupported json type." +
        std::string(json.type_name()));
  }
}

LuaScript::Variant::~Variant() {
  // destroy the initialized child
  if (_type == Type::STRING) {
    _string.~basic_string();
  } else if (_type == Type::FUNCTION) {
    _function.~basic_string();
  } else if (_type == Type::CFUNCTION) {
    _c_function.~basic_string();
  } else if (_type == Type::TABLE) {
    _table.~unordered_map();
  }
}

nlohmann::json LuaScript::Variant::toJson() const {
  nlohmann::json json;
  switch (_type) {
    case Type::NIL:
      json = nullptr;
      break;
    case Type::BOOLEAN:
      json = _boolean;
      break;
    case Type::NUMBER:
      json = _number;
      break;
    case Type::STRING:
      json = _string;
      break;
    case Type::TABLE:
      for (auto it : _table) {
        std::string key = "";
        switch (it.first->type()) {
          case Type::BOOLEAN:
            key = std::to_string(it.first->boolean());
            break;
          case Type::NUMBER:
            key = std::to_string(it.first->number());
            break;
          case Type::STRING:
            key = it.first->string();
            break;
          default:
            throw std::runtime_error(
                "LuaScript::Variant::toJson: Unable to convert from type " +
                std::string(TYPE_NAMES[int(it.first->type())]) +
                " to a string for conversion to json.");
        }

        json[key] = it.second->toJson();
      }
      break;
    default:
      throw std::runtime_error(
          "LuaScript::Variant::toJson : Unable to convert type " +
          std::string(typeName()) + " to json.");
  }
  return json;
}

void LuaScript::Variant::push(lua_State *state) const {
  lua_checkstack(state, 1);
  switch (_type) {
    case Type::NIL:
      lua_pushnil(state);
      break;
    case Type::BOOLEAN:
      lua_pushboolean(state, _boolean);
      break;
    case Type::NUMBER:
      lua_pushnumber(state, _number);
      break;
    case Type::STRING:
      lua_pushstring(state, _string.c_str());
      break;
    case Type::USERDATA:
      lua_pushlightuserdata(state, _userdata);
      break;
    case Type::TABLE:
      lua_createtable(state, 0, _table.size());
      for (auto it : _table) {
        it.first->push(state);
        it.second->push(state);
        lua_settable(state, -2);
      }
      break;
    default:
      LOG_WARN << "Unable to push type " << int(_type) << " onto the stack"
               << LOG_END;
  }
}

LuaScript::Type LuaScript::Variant::type() const { return _type; }

const char *LuaScript::Variant::typeName() const {
  return TYPE_NAMES[int(_type)];
}

double LuaScript::Variant::number() const { return _number; }
double &LuaScript::Variant::number() { return _number; }
double &LuaScript::Variant::asNumber() {
  if (_type != Type::NUMBER) {
    throw std::runtime_error(
        "LuaScript::Variant::asNumber: Variant is not a number");
  }
  return _number;
}

const std::string &LuaScript::Variant::string() const { return _string; }
std::string &LuaScript::Variant::string() { return _string; }
std::string &LuaScript::Variant::asString() {
  if (_type != Type::STRING) {
    throw std::runtime_error(
        "LuaScript::Variant::asString: Variant is not a string");
  }
  return _string;
}

void *LuaScript::Variant::userdata() { return _userdata; }
const void *LuaScript::Variant::userdata() const { return _userdata; }
void *LuaScript::Variant::asUserdata() {
  if (_type != Type::USERDATA) {
    throw std::runtime_error(
        "LuaScript::Variant::asUserdata: Variant is not userdata");
  }
  return _userdata;
}

const LuaScript::Variant::Table &LuaScript::Variant::table() const {
  return _table;
}
LuaScript::Variant::Table &LuaScript::Variant::table() { return _table; }
LuaScript::Variant::Table &LuaScript::Variant::asTable() {
  if (_type != Type::TABLE) {
    throw std::runtime_error(
        "LuaScript::Variant::asTable: Variant is not a table");
  }
  return _table;
}

bool &LuaScript::Variant::boolean() { return _boolean; }
const bool &LuaScript::Variant::boolean() const { return _boolean; }
bool &LuaScript::Variant::asBoolean() {
  if (_type != Type::BOOLEAN) {
    throw std::runtime_error(
        "LuaScript::Variant::asBoolean: Variant is not a boolean");
  }
  return _boolean;
}

void LuaScript::Variant::operator=(Variant &&other) {
  this->~Variant();
  new (this) Variant(other);
}

void LuaScript::Variant::operator=(const Variant &other) {
  this->~Variant();
  new (this) Variant(other);
}

// StackGuard
// =============================================================================

LuaScript::StackGuard::StackGuard(const LuaScript *script,
                                  const std::string &name)
    : _stack_size(script->stackSize()), _script(script), _name(name) {}

LuaScript::StackGuard::~StackGuard() noexcept(false) {
  int current_size = _script->stackSize();
  if (current_size != _stack_size) {
    throw std::runtime_error("A LuaScript function " + _name +
                             " is not stack balanced. Started at " +
                             std::to_string(_stack_size) + " but is " +
                             std::to_string(current_size));
  }
}
