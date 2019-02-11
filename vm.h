#pragma once

#include "common.h"
#include "lexer.h"

namespace nanoexpr::vm
{
  using nullary_operation = std::function<Value()>;
  using unary_operation = std::function<Value(Value)>;
  using binary_operation = std::function<Value(Value, Value)>;
  using ternary_operation = std::function<Value(Value, Value, Value)>;
  using lambda_t = std::function<Value()>;

  class Signature
  {
  public:
    std::array<ValueType, 3> types;
    ValueType returnType;

    Signature(ValueType retType, ValueType a1 = ValueType::NONE, ValueType a2 = ValueType::NONE, ValueType a3 = ValueType::NONE)
      : returnType(retType), types({ a1, a2, a3 }) { }

    bool operator==(const Signature& other) const { return types == other.types; }
  };

  using opcode_t = std::string;

  struct VariantFunctor
  {
    size_t args;

    union
    {
      nullary_operation nullary;
      unary_operation unary;
      binary_operation binary;
      ternary_operation ternary;
    };

    VariantFunctor(nullary_operation nullary) : args(1), nullary(nullary) { }
    VariantFunctor(unary_operation unary) : args(1), unary(unary) { }
    VariantFunctor(binary_operation binary) : args(2), binary(binary) { }
    VariantFunctor(ternary_operation binary) : args(2), ternary(binary) { }
    VariantFunctor(const VariantFunctor& o) { this->operator=(o); }

    VariantFunctor& operator=(const VariantFunctor& o)
    {
      this->args = o.args;

      switch (args) {
        case 0: new (&nullary) nullary_operation(); this->nullary = o.nullary; break;
        case 1: new (&unary) unary_operation(); this->unary = o.unary; break;
        case 2: new (&binary) binary_operation(); this->binary = o.binary; break;
        case 3: new (&ternary) ternary_operation(); this->ternary = o.ternary; break;
      }

      return *this;
    }

    ~VariantFunctor()
    {
      switch (args) {
        case 0: nullary.~nullary_operation(); break;
        case 1: unary.~unary_operation(); break;
        case 2: binary.~binary_operation(); break;
        case 3: ternary.~ternary_operation(); break;
      }
    }
  };

  struct FunctionDefinition
  {
    Signature signature;
    VariantFunctor function;
    bool isConstant;
  };

  class Functions
  {
  private:
    void init();

  protected:
    std::unordered_map<opcode_t, std::vector<FunctionDefinition>> functors;

    template<bool Cmp, template<typename TT> typename F> void registerNumericBinary(const vm::opcode_t& opcode)
    {
      registerBinary(opcode, Cmp ? ValueType::BOOL : ValueType::INTEGRAL, ValueType::INTEGRAL, ValueType::INTEGRAL, [](Value v1, Value v2) { return Value(F<integral_t>()(v1.i(), v2.i())); });
      registerBinary(opcode, Cmp ? ValueType::BOOL : ValueType::REAL, ValueType::REAL, ValueType::REAL, [](Value v1, Value v2) { return Value(F<real_t>()(v1.r(), v2.r())); });
    }

    template<bool Cmp, template<typename TT> typename F> void registerNumericUnary(const vm::opcode_t& opcode)
    {
      registerUnary(opcode, Cmp ? ValueType::BOOL : ValueType::INTEGRAL, ValueType::INTEGRAL, [](Value v) { return Value(F<integral_t>()(v.i())); });
      registerUnary(opcode, Cmp ? ValueType::BOOL : ValueType::REAL, ValueType::REAL, [](Value v) { return Value(F<real_t>()(v.r())); });
    }

  public:
    void registerTernary(const opcode_t& opcode, ValueType returnType, ValueType arg1, ValueType arg2, ValueType arg3, bool isConstant, ternary_operation functor) { functors[opcode].push_back({ Signature(returnType, arg1, arg2, arg3), functor, true }); }
    void registerBinary(const opcode_t& opcode, ValueType returnType, ValueType arg1, ValueType arg2, binary_operation functor) { functors[opcode].push_back({ Signature(returnType, arg1, arg2), functor, true }); }
    void registerUnary(const opcode_t& opcode, ValueType returnType, ValueType arg1, unary_operation functor) { functors[opcode].push_back({ Signature(returnType, arg1), functor, true }); }
    void registerNullary(const opcode_t& opcode, ValueType returnType, bool isConstant, nullary_operation functor) { functors[opcode].push_back({ Signature(returnType), functor, isConstant }); }

  public:
    /* TODO: could deduce signature from R and T */
    template<typename T, typename R, R(*func)(T)> void registerFreeUnaryFunction(const vm::opcode_t& opcode, ValueType returnType, ValueType arg1)
    {
      registerUnary(opcode, returnType, arg1, [](Value v) { return Value(func(v.as<T>())); });
    }

    /* TODO: could deduce signature from R and T */
    template<typename T, typename R, typename U, R(*func)(T, U)> void registerFreeBinaryFunction(const vm::opcode_t& opcode, ValueType returnType, ValueType arg1, ValueType arg2)
    {
      registerBinary(opcode, returnType, arg1, arg2, [](Value v1, Value v2) { return Value(func(v1.as<T>(), v2.as<T>())); });
    }

  public:
    const FunctionDefinition* find(const opcode_t& opcode, ValueType t1 = ValueType::NONE, ValueType t2 = ValueType::NONE, ValueType t3 = ValueType::NONE) const
    {
      const auto& functorsByOpcode = functors.find(opcode);

      if (functorsByOpcode != functors.end())
      {
        Signature actual = Signature(ValueType::NONE, t1, t2, t3);
        auto it = std::find_if(functorsByOpcode->second.begin(), functorsByOpcode->second.end(), [&actual](const auto& definition) { return definition.signature == actual; });

        if (it != functorsByOpcode->second.end())
          return &(*it);
      }

      return nullptr;
    }

    Functions() noexcept { init(); }
  };

  class Enums
  {
  public:
    using utype = integral_t;
    using enum_map_t = std::unordered_map<std::string, std::unordered_map<std::string, utype>>;

    template<typename T> using typed_pair = std::pair<std::string_view, T>;
    using init_pair = std::pair<std::string_view, uint64_t>;

  private:
    enum_map_t mapping;

  public:
    template<typename T> void registerEnum(const std::string& name, const std::initializer_list<typed_pair<T>>& entries)
    {
      auto& data = mapping[name];

      std::transform(entries.begin(), entries.end(), std::inserter(data, data.end()), [](const typed_pair<T>& pair) {
        return std::make_pair(std::string(pair.first), static_cast<utype>(pair.second));
      });
    }

    std::pair<bool, Value> findEnum(const std::string& name, const std::string& valueName) const
    {
      auto data = mapping.find(name);

      if (data != mapping.end())
      {
        auto value = data->second.find(valueName);
        if (value != data->second.end())
          return std::make_pair(true, Value(value->second));

      }

      return std::make_pair(false, Value(0));
    }


  };

  class Engine;

  class Environment
  {
  private:
    const Engine& _engine;

    std::unordered_map<std::string, TypedValue> _variables;

  protected:
    Environment(const Engine& engine) : _engine(engine) { }

  public:
    void set(const std::string& ident, TypedValue value) { _variables.emplace(std::make_pair(ident, value)); }
    const TypedValue* get(const std::string& identifier) const;
 
    const Engine& engine() const { return _engine; }

    friend class Engine;
  };

  class Engine
  {
  private:
    Functions _functions;
    Enums _enums;
    ValueType _customTypeMapping;
    std::unordered_map<std::string, TypedValue> _globals;

  public:
    Engine() : _customTypeMapping(ValueType::FIRST_CUSTOM_TYPE) { }

    const FunctionDefinition* findFunction(const opcode_t& opcode, ValueType t1, ValueType t2 = ValueType::NONE, ValueType t3 = ValueType::NONE) const { return _functions.find(opcode, t1, t2, t3); }
    auto findEnum(const std::string& name, const std::string& value) const { return _enums.findEnum(name, value); }

    Enums& enums() { return _enums; }
    Functions& functions() { return _functions; }

    template<typename T> void setGlobal(const std::string& identifier, T value) { _globals.emplace(std::make_pair(identitifer, value)); }
    const TypedValue* getGlobal(const std::string& identifier) const { auto it = _globals.find(identifier); return it != _globals.end() ? &it->second : nullptr; }

    template<typename T> ValueType mapCustomType() { ValueType current = _customTypeMapping; _customTypeMapping = (ValueType)((std::underlying_type_t<ValueType>)_customTypeMapping + 1); return current; }

    Environment createEnvironment() { return Environment(*this); }
  };

  const TypedValue* Environment::get(const std::string& identifier) const {
    auto it = _variables.find(identifier);
    if (it != _variables.end())
      return &it->second;

    return _engine.getGlobal(identifier);
  }
}

void nanoexpr::vm::Functions::init()
{
  using V = nanoexpr::Value;
  using VT = nanoexpr::ValueType;
  using real_t = nanoexpr::real_t;
  using integral_t = nanoexpr::integral_t;

  /* numerical */
  registerNumericBinary<false, std::plus>("+");
  registerNumericBinary<false, std::minus>("-");
  registerNumericBinary<false, std::multiplies>("*");
  registerNumericBinary<false, std::divides>("/");
  registerBinary("%", VT::INTEGRAL, VT::INTEGRAL, VT::INTEGRAL, [](V v1, V v2) { return std::modulus<integral_t>()(v1.i(), v2.i()); });
  registerNumericUnary<false, std::negate>("-");

  /* bitwise */
  registerUnary("~", VT::INTEGRAL, VT::INTEGRAL, [](V v) { return std::bit_not<integral_t>()(v.i()); });
  registerBinary("&", VT::INTEGRAL, VT::INTEGRAL, VT::INTEGRAL, [](V v1, V v2) { return std::bit_and<integral_t>()(v1.b(), v2.b()); });
  registerBinary("|", VT::INTEGRAL, VT::INTEGRAL, VT::INTEGRAL, [](V v1, V v2) { return std::bit_or<integral_t>()(v1.b(), v2.b()); });

  /* equality */
  registerNumericBinary<true, std::equal_to>("==");
  registerNumericBinary<true, std::not_equal_to>("!=");

  /* comparison */
  registerNumericBinary<true, std::less>("<");
  registerNumericBinary<true, std::less_equal>("<=");
  registerNumericBinary<true, std::greater>(">");
  registerNumericBinary<true, std::greater_equal>("<=");

  /* logical operators */
  registerBinary("&&", VT::BOOL, VT::BOOL, VT::BOOL, [](V v1, V v2) { return std::logical_and<bool>()(v1.b(), v2.b()); });
  registerBinary("||", VT::BOOL, VT::BOOL, VT::BOOL, [](V v1, V v2) { return std::logical_or<bool>()(v1.b(), v2.b()); });
  registerBinary("==", VT::BOOL, VT::BOOL, VT::BOOL, [](V v1, V v2) { return std::equal_to<bool>()(v1.b(), v2.b()); });
  registerBinary("!=", VT::BOOL, VT::BOOL, VT::BOOL, [](V v1, V v2) { return std::not_equal_to<bool>()(v1.b(), v2.b()); });
  registerUnary("!", VT::BOOL, VT::BOOL, [](V v) { return std::logical_not<bool>()(v.b()); });

  /* builtins */
  registerBinary("min", VT::INTEGRAL, VT::INTEGRAL, VT::INTEGRAL, [](V v1, V v2) { return std::min(v1.i(), v2.i()); });
  registerBinary("min", VT::REAL, VT::REAL, VT::REAL, [](V v1, V v2) { return std::min(v1.r(), v2.r()); });
  registerBinary("max", VT::INTEGRAL, VT::INTEGRAL, VT::INTEGRAL, [](V v1, V v2) { return std::max(v1.i(), v2.i()); });
  registerBinary("max", VT::REAL, VT::REAL, VT::REAL, [](V v1, V v2) { return std::max(v1.r(), v2.r()); });
  registerTernary("clamp", VT::REAL, VT::REAL, VT::REAL, VT::REAL, true, [](V min, V v2, V max) { return v2.r() < min.r() ? min.r() : (v2.r() > max.r() ? max : v2.r()); });

  registerNullary("rand", VT::INTEGRAL, false, [] { return Value(rand() % RAND_MAX); });
  registerFreeUnaryFunction<real_t, real_t, std::abs>("abs", VT::REAL, VT::REAL);
  registerFreeUnaryFunction<integral_t, integral_t, std::abs>("abs", ValueType::INTEGRAL, VT::INTEGRAL);
  registerFreeUnaryFunction<real_t, real_t, std::sqrt>("sqrt", VT::REAL, VT::REAL);
  registerFreeUnaryFunction<real_t, real_t, std::cbrt>("cbrt", VT::REAL, VT::REAL);


  if (nanoexpr::config::EnableTrigonometricFunctions)
  {
    registerFreeUnaryFunction<real_t, real_t, std::cos>("cos", VT::REAL, VT::REAL);
    registerFreeUnaryFunction<real_t, real_t, std::sin>("sin", VT::REAL, VT::REAL);
    registerFreeUnaryFunction<real_t, real_t, std::tan>("tan", VT::REAL, VT::REAL);
    registerFreeUnaryFunction<real_t, real_t, std::acos>("acos", VT::REAL, VT::REAL);
    registerFreeUnaryFunction<real_t, real_t, std::acos>("asin", VT::REAL, VT::REAL);
    registerFreeUnaryFunction<real_t, real_t, std::atan>("atan", VT::REAL, VT::REAL);
    registerFreeBinaryFunction<real_t, real_t, real_t, std::atan2>("atan", VT::REAL, VT::REAL, VT::REAL);
  }

  if (nanoexpr::config::EnableHyperbolicFunctions)
  {
    registerFreeUnaryFunction<real_t, real_t, std::cosh>("cosh", VT::REAL, VT::REAL);
    registerFreeUnaryFunction<real_t, real_t, std::sinh>("sinh", VT::REAL, VT::REAL);
    registerFreeUnaryFunction<real_t, real_t, std::tanh>("tanh", VT::REAL, VT::REAL);
    registerFreeUnaryFunction<real_t, real_t, std::acosh>("acosh", VT::REAL, VT::REAL);
    registerFreeUnaryFunction<real_t, real_t, std::acosh>("asinh", VT::REAL, VT::REAL);
    registerFreeUnaryFunction<real_t, real_t, std::atanh>("atanh", VT::REAL, VT::REAL);
  }
}