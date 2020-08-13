#pragma once

#include <functional>
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

#include <nlohmann/json.hpp>

struct lua_State;

class LuaScript {
 public:
  enum class Type {
    NIL,
    BOOLEAN,
    NUMBER,
    STRING,
    FUNCTION,
    CFUNCTION,
    USERDATA,
    TABLE
  };

  static const char *TYPE_NAMES[8];

  /**
   * @brief Represents a variable in lua.
   */
  class Variant {
   public:
    Variant();
    Variant(double number);
    Variant(bool boolean);
    Variant(const std::string &string);
    Variant(void *userdata);
    Variant(
        const std::unordered_map<std::string, std::shared_ptr<Variant>> table);
    Variant(lua_State *state, int idx = -1);

    Variant(const Variant &other);
    Variant(Variant &&other);

    Variant(const nlohmann::json &json);

    virtual ~Variant();

    void operator=(const Variant &other);
    void operator=(Variant &&other);

    nlohmann::json toJson() const;

    /**
     * @brief Pushes this variant onto the stack
     */
    void push(lua_State *state) const;

    Type type() const;
    const char *typeName() const;

    double number() const;
    double &number();

    const std::string &string() const;
    std::string &string();

    void *userdata();
    const void *userdata() const;

    bool &boolean();
    const bool &boolean() const;

    const std::unordered_map<std::string, std::shared_ptr<Variant>> &table()
        const;
    std::unordered_map<std::string, std::shared_ptr<Variant>> &table();

   private:
    Type _type;
    union {
      double _number;
      bool _boolean;
      std::string _string;
      std::string _function;
      std::string _c_function;
      void *_userdata;
      std::unordered_map<std::string, std::shared_ptr<Variant>> _table;
    };
  };

 private:
  struct LuaFunction {
    std::function<Variant(std::vector<Variant>)> handler;
    std::vector<Type> arguments;
    std::string name;
  };

  class StackGuard {
  public:
    StackGuard(const LuaScript *script, const std::string &name = "");
    virtual ~StackGuard() noexcept(false);

  private:
    int _stack_size;
    const LuaScript *_script;
    std::string _name;
  };

 public:
  /**
   * @brief Stores a pointer to this script in the lua registry
   */
  static const std::string REGISTRY_THIS;

  LuaScript();
  LuaScript(const std::string &path);
  LuaScript(const LuaScript &other) = delete;
  LuaScript(LuaScript &&other);

  virtual ~LuaScript();

  void operator=(const LuaScript &other) = delete;
  void operator=(LuaScript &&other);

  /** @brief Loads the lua standard libraries. */
  void loadStandardLibs();

  /** @brief Returns the global or NIL if the global doesn't exist. */
  Variant global(const std::string &s) const;

  /** @brief Adds the value into the global context with name s. */
  void setGlobal(const std::string &s, Variant value);

  /** @brief Calls the given function. Returns the functions return value. */
  std::vector<Variant> call(const std::string &function,
                            const std::vector<Variant> args = {});

  /**
   * @brief Calls the given function. Returns the functions return value.
   * The arguments are converted into a table and given to the function as a
   * single argument
   * // TODO: Remove this function once tables are properly supported
   */
  std::vector<Variant> callVarArg(const std::string &function,
                                  const std::vector<Variant> &args);

  /**
   * @brief Can be called from within functions called from lua to trigger a
   * lua error.
   */
  void error(const std::string &text);

  /**
   * @brief Registers a global function in the lua namespace. The handler will
   *        be executed when the function is called within lua.
   * @param arguments An optional vector that fixes the types of the arguments.
   *                  This does not restrict the number of arguments in the
   *                  function call.
   */
  void registerFunction(const std::string name,
                        std::function<Variant(std::vector<Variant>)> handler,
                        const std::vector<Type> &arguments = {});

  Variant onFunctionCall(int index, std::vector<Variant> arguments);

  int stackSize() const;

 private:
  void load(const std::string &path);

  lua_State *_lua_state;
  std::vector<LuaFunction> _function_handlers;
};
