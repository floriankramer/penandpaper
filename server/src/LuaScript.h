#pragma once

#include <functional>
#include <memory>
#include <nlohmann/json.hpp>
#include <optional>
#include <string>
#include <unordered_map>
#include <vector>

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

  class Variant;
  typedef std::shared_ptr<Variant> VariantPtr;

  class Table {
   public:
    Table();
    std::optional<VariantPtr> operator[](const std::string &key) const;
    std::optional<VariantPtr> operator[](const char *key) const;
    std::optional<VariantPtr> operator[](bool key) const;
    std::optional<VariantPtr> operator[](double key) const;
    std::optional<VariantPtr> operator[](size_t key) const;

    void insert(const std::string &key, const VariantPtr &ptr);
    void insert(bool key, const VariantPtr &ptr);
    void insert(double key, const VariantPtr &ptr);

    void push_back(const VariantPtr &ptr);
    void pop();

    VariantPtr &front();
    VariantPtr &back();

    std::vector<VariantPtr>::iterator vectorBegin();
    std::vector<VariantPtr>::const_iterator vectorBegin() const;

    std::vector<VariantPtr>::iterator vectorEnd();
    std::vector<VariantPtr>::const_iterator vectorEnd() const;

    std::unordered_map<double, VariantPtr>::iterator numberBegin();
    std::unordered_map<double, VariantPtr>::const_iterator numberBegin() const;

    std::unordered_map<double, VariantPtr>::iterator numberEnd();
    std::unordered_map<double, VariantPtr>::const_iterator numberEnd() const;

    std::unordered_map<std::string, VariantPtr>::iterator stringBegin();
    std::unordered_map<std::string, VariantPtr>::const_iterator stringBegin()
        const;

    std::unordered_map<std::string, VariantPtr>::iterator stringEnd();
    std::unordered_map<std::string, VariantPtr>::const_iterator stringEnd()
        const;

    size_t vectorSize() const;
    size_t unorderedSize() const;

    void consolidateNumberEntries();

   private:
    std::unordered_map<std::string, VariantPtr> _string_entries;
    std::array<VariantPtr, 2> _bool_entries;
    std::unordered_map<double, VariantPtr> _number_entries;

    std::vector<VariantPtr> _vector_entries;
  };

  /**
   * @brief Represents a variable in lua.
   */
  class Variant {
   public:
    typedef std::shared_ptr<Variant> VariantPtr;

    Variant();
    Variant(double number);
    Variant(bool boolean);
    Variant(const std::string &string);
    Variant(void *userdata);
    Variant(const Table table);
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
    const double &asNumber() const;
    double &number();
    double &asNumber();

    const std::string &string() const;
    const std::string &asString() const;
    std::string &string();
    std::string &asString();

    void *userdata();
    void *asUserdata();
    const void *userdata() const;
    const void *asUserdata() const;

    bool &boolean();
    bool &asBoolean();
    const bool &boolean() const;
    const bool &asBoolean() const;

    const Table &table() const;
    const Table &asTable() const;
    Table &table();
    Table &asTable();

   private:
    Type _type;
    union {
      double _number;
      bool _boolean;
      std::string _string;
      uint64_t _function;
      std::string _c_function;
      void *_userdata;
      Table _table;
    };

    uint64_t *_function_references;
    // For accessing a function we need a lua state. This is only defined for
    // Type::FUNCTION objects.
    lua_State *_function_state;

    static const uint64_t REGISTRY_MASTER_KEY;
    static const char *FUNCTION_TABLE_KEY;
    static const char *FUNCTION_INDEX_KEY;
  };

  typedef std::function<Variant(LuaScript *script, std::vector<Variant>)>
      ApiFunction;

 private:
  struct LuaFunction {
    ApiFunction handler;
    std::vector<Type> arguments;
    std::string name;
  };

  class StackGuard {
   public:
    StackGuard(lua_State *lua, const std::string &name = "");
    virtual ~StackGuard() noexcept(false);

   private:
    int _stack_size;
    lua_State *_lua;
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
  void registerFunction(const std::string name, ApiFunction handler,
                        const std::vector<Type> &arguments = {});

  Variant onFunctionCall(int index, std::vector<Variant> arguments);

  int stackSize() const;

 private:
  void load(const std::string &path);

  lua_State *_lua_state;
  std::vector<LuaFunction> _function_handlers;
};
