#include <cstdint>
#include <unordered_map>
#include <unordered_set>
#include <string>
#include <string_view>
#include <regex>
#include <vector>
#include <memory>
#include <array>
#include <functional>
#include <cctype>

namespace nanoexpr
{
  /* configuration */
  namespace config
  {
    using real_t = float;
    using integral_t = int32_t;
    static constexpr bool EnableIntegerPromotionToReal = true;
    static constexpr bool EnableTrigonometricFunctions = true;
    static constexpr bool EnableHyperbolicFunctions = true;
    static constexpr bool OptimizationConstantFolding = true;
  }


  


  using real_t = config::real_t;
  using integral_t = config::integral_t;

  enum class ValueType
  {
    REAL,
    INTEGRAL,
    BOOL,
    POINTER,

    FIRST_ENUM = 512,
    LAST_ENUM = 1024,

    NONE
  };

  template<ValueType T> struct value_traits { };
  template<> struct value_traits<ValueType::REAL> { using type = real_t; };
  template<> struct value_traits<ValueType::INTEGRAL> { using type = integral_t; };
  template<> struct value_traits<ValueType::BOOL> { using type = bool; };
  template<> struct value_traits<ValueType::POINTER> { using type = void*; };

  union Value
  {
    real_t real;
    integral_t integral;
    bool boolean;
    void* ptr;

    Value(integral_t integral) : integral(integral) { }
    Value(real_t real) : real(real) { }
    Value(bool boolean) : boolean(boolean) { }
    template<typename T> Value(T* ptr) : ptr(ptr) { }

    inline integral_t i() const { return integral; }
    inline real_t r() const { return real; }
    inline bool b() const { return boolean; }
    template<typename T> inline T* p() const { return static_cast<T*>(ptr); }

    template<typename T, typename std::enable_if<!std::is_enum<T>::value, int>::type* = nullptr> T as() const;
    template<typename T, typename std::enable_if<std::is_enum<T>::value, int>::type* = nullptr> inline T as() const { return static_cast<T>(as<integral_t>()); }

    template<ValueType T> typename value_traits<T>::type as() { return as<typename value_traits<T>::type>(); }
  };

  template<> real_t Value::as() const { return real; }
  template<> integral_t Value::as() const { return integral; }
  template<> bool Value::as() const { return real; }

  struct TypedValue
  {
    Value value;
    ValueType type;

    TypedValue(real_t real) : value(real), type(ValueType::REAL) { }
    TypedValue(integral_t real) : value(real), type(ValueType::INTEGRAL) { }
    TypedValue(bool real) : value(real), type(ValueType::BOOL) { }
  };

  namespace lex
  {
    enum class TokenType
    {
      NONE, SKIP,

      IDENTIFIER,
      STRING, FLOAT, INTEGRAL, BOOLEAN,
      OPERATOR, SYMBOL
    };

    class Token
    {
      TokenType _type;
      std::string _textual;

      union
      {
        Value _value;
      };

    public:
      Token(TokenType type) : _type(type) { }
      Token(TokenType type, std::string_view textual) : _type(type), _textual(textual) { }
      template<typename T, typename std::enable_if<!std::is_enum<T>::value>::type* = nullptr> Token(TokenType type, std::string_view textual, T value) : _type(type), _textual(textual), _value(value) { }

      TokenType type() const { return _type; }
      size_t length() const { return _textual.length(); }
      const Value& value() const { return _value; }
      const auto& textual() const { return _textual; }
      std::string string() const { return _textual.substr(1, _textual.length() - 2); }

      bool valid() const { return _type != TokenType::NONE; }

      bool match(TokenType type, const std::string& textual) const { return _type == type && _textual == textual; }
    };

    class Rule
    {
    protected:
      bool matches(std::string_view input, std::string_view expected) const
      {
        const bool longEnough = input.length() >= expected.length();

        if (longEnough)
        {
          const bool isMatching = input.substr(0, expected.length()) == expected;
          return isMatching && hasSpaceOrDigitTermination(input, expected.length());
        }

        return false;
      }

      bool hasSpaceOrNonDigitTermination(std::string_view input, size_t position) const
      {
        return input.length() == position || !std::isdigit(input[position]);
      }

      bool hasSpaceOrDigitTermination(std::string_view input, size_t position) const
      {
        return input.length() == position || !std::isalpha(input[position]);
      }

      std::string_view fetch(std::string_view input) const
      {
        size_t length = 0;
        while (length < input.length() && !std::isspace(input[length]) && !std::isdigit(input[length])) ++length;
        return input.substr(0, length);
      }

    public:
      virtual Token matches(const std::string_view input) const = 0;
    };

    class WhiteSpaceRule : public Rule
    {
    public:
      Token matches(const std::string_view input) const override
      {
        size_t length = 0;
        while (length < input.length() && std::isspace(input[length]))
          ++length;

        if (length > 0)
          return Token(TokenType::SKIP, input.substr(0, length));
        else
          return Token(TokenType::NONE);
      }

    };

    class KeywordRule : public Rule
    {
    private:
      TokenType _type;
      std::string _keyword;

    public:
      KeywordRule(TokenType type, const std::string& keyword) : _type(type), _keyword(keyword) { }

      Token matches(const std::string_view input) const override
      {
        if (Rule::matches(input, _keyword))
          return Token(_type, _keyword);
        else
          return TokenType::NONE;
      }
    };

    class IdentifierRule : public Rule
    {
    public:
      Token matches(const std::string_view input) const override
      {
        size_t p = 0;

        

        while (p < input.length())
        {
          auto c = input[p];

          /* can't start with digit */
          if (p == 0 && std::isdigit(c))
            break;
          else if (std::isalnum(c) || std::isdigit(c) || c == '_')
            ++p;
          else
            break;
        }

        if (p > 0)
          return Token(TokenType::IDENTIFIER, input.substr(0, p));
        else
          return TokenType::NONE;
      }

    };

    class StringRule : public Rule
    {
      static constexpr char delim = '\'';
    public:
      Token matches(const std::string_view input) const override
      {
        size_t p = 0;

        if (input[p] == delim)
        {
          ++p;

          while (true)
          {
            if (p >= input.length()) /* end before closing string*/
              return TokenType::NONE;
            else if (p == delim)
              return Token(TokenType::STRING, input.substr(0, p));
            else //TODO: manage escape \delim
              ++p;
          }
        }

        return TokenType::NONE;
      }
    };

    class FloatingRule : public Rule
    {
    public:
      Token matches(const std::string_view input) const override
      {
        size_t s = 0, p = 0;
        bool finished = false, failed = false;

        while (p < input.length() && !finished)
        {
          auto c = input[p];

          if (s == 0)
          {
            if (c == '-' || c == '+')
              ++s;
            else if (std::isdigit(c)) { ++s; --p; }
            else { failed = true; finished = true; }
          }
          else if (s == 1)
          {
            if (std::isdigit(c));
            else if (c == '.') { ++s; }
            else { failed = true; finished = true; }
          }
          else if (s == 2)
          {
            if (Rule::hasSpaceOrNonDigitTermination(input, p))
            {
              finished = true;
              break;
            }
          }

          ++p;
        }

        if (!failed)
        {
          std::string copy = std::string(input.substr(0, p));
          return Token(TokenType::FLOAT, copy, std::stof(copy));
        }
        else
          return TokenType::NONE;
      }
    };

    class IntegerRule : public Rule
    {
    public:
      Token matches(const std::string_view input) const override
      {
        bool hasSign = false;
        size_t p = 0;

        if (input[p] == '-' /*|| input[p] == '+'*/)
        {
          hasSign = true;
          ++p;
        }

        while (p < input.length() && std::isdigit(input[p]))
          ++p;

        if (p > 0 && (!hasSign || p > 1) && Rule::hasSpaceOrNonDigitTermination(input, p))
        {
          std::string copy = std::string(input.substr(0, p));
          return Token(TokenType::INTEGRAL, input.substr(0, p), std::stoi(copy)); //TODO: stoi, choose according to integer_t type?
        }

        return TokenType::NONE;
      }
    };

    class BooleanRule : public Rule
    {
    public:
      Token matches(const std::string_view input) const override
      {
        if (Rule::matches(input, "false"))
          return Token(TokenType::BOOLEAN, "false", false);
        else if (Rule::matches(input, "true"))
          return Token(TokenType::BOOLEAN, "true", true);
        else
          return TokenType::NONE;
      }
    };

    class OperatorRule : public Rule
    {
      using mapping_t = std::unordered_set<std::string>;

    private:
      bool _requireSpacing;
      TokenType _type;
      mapping_t _mapping;

    public:
      OperatorRule(TokenType type, bool requireSpacing, const std::initializer_list<std::string>& mapping) : _type(type), _requireSpacing(requireSpacing), _mapping(mapping)
      {

      }

      Token matches(const std::string_view input) const override
      {
        if (_requireSpacing)
        {
          std::string token = std::string(Rule::fetch(input));

          if (token.length() > 0)
          {
            auto it = _mapping.find(token);

            if (it != _mapping.end())
              return Token(_type, *it);
          }
        }
        else
        {
          for (const typename mapping_t::value_type& entry : _mapping)
          {
            if (input.length() >= entry.length())
            {
              if (input.substr(0, entry.length()) == entry)
                return Token(_type, entry);
            }
          }
        }



        return TokenType::NONE;
      }
    };

    using token_list = std::vector<Token>;

    struct LexerResult
    {
      token_list tokens;
      bool success;
      std::string message;
    };


    class Lexer
    {
    private:
      std::vector<std::unique_ptr<Rule>> rules;

    public:
      Lexer();
      LexerResult parse(const std::string& text);
    };
  }

  namespace vm
  {
    using nullary_operation = std::function<Value()>;
    using unary_operation = std::function<Value(Value)>;
    using binary_operation = std::function<Value(Value, Value)>;
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
      };

      VariantFunctor(nullary_operation nullary) : args(1), nullary(nullary) { }
      VariantFunctor(unary_operation unary) : args(1), unary(unary) { }
      VariantFunctor(binary_operation binary) : args(2), binary(binary) { }
      VariantFunctor(const VariantFunctor& o) { this->operator=(o); }

      VariantFunctor& operator=(const VariantFunctor& o)
      {
        this->args = o.args;

        switch (args) {
          case 0: new (&nullary) nullary_operation(); this->nullary = o.nullary; break;
          case 1: new (&unary) unary_operation(); this->unary = o.unary; break;
          case 2: new (&binary) binary_operation(); this->binary = o.binary; break;
        }

        return *this;
      }

      ~VariantFunctor()
      {
        switch (args) {
          case 0: nullary.~nullary_operation(); break;
          case 1: unary.~unary_operation(); break;
          case 2: binary.~binary_operation(); break;
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

    public:
      std::unordered_map<opcode_t, std::vector<FunctionDefinition>> functors;

      void registerBinary(const opcode_t& opcode, Signature signature, binary_operation functor) { functors[opcode].push_back({ signature, functor, true }); }
      void registerUnary(const opcode_t& opcode, Signature signature, unary_operation functor) { functors[opcode].push_back({ signature, functor, true }); }

      template<bool Cmp, template<typename TT> typename F> void registerNumericBinary(const vm::opcode_t& opcode)
      {
        registerBinary(opcode, Signature(Cmp ? ValueType::BOOL : ValueType::INTEGRAL, ValueType::INTEGRAL, ValueType::INTEGRAL), [](Value v1, Value v2) { return Value(F<integral_t>()(v1.i(), v2.i())); });
        registerBinary(opcode, Signature(Cmp ? ValueType::BOOL : ValueType::REAL, ValueType::REAL, ValueType::REAL), [](Value v1, Value v2) { return Value(F<real_t>()(v1.r(), v2.r())); });
      }

      template<bool Cmp, template<typename TT> typename F> void registerNumericUnary(const vm::opcode_t& opcode)
      {
        registerUnary(opcode, Signature(Cmp ? ValueType::BOOL : ValueType::INTEGRAL, ValueType::INTEGRAL), [](Value v) { return Value(F<integral_t>()(v.i())); });
        registerUnary(opcode, Signature(Cmp ? ValueType::BOOL : ValueType::REAL, ValueType::REAL), [](Value v) { return Value(F<real_t>()(v.r())); });
      }

      /* TODO: could deduce signature from R and T */
      template<typename T, typename R, R(*func)(T)> void registerFreeUnaryFunction(const vm::opcode_t& opcode, ValueType returnType, ValueType arg1)
      {
        registerUnary(opcode, Signature(returnType, arg1), [](Value v) { return Value(func(v.as<T>())); });
      }

      /* TODO: could deduce signature from R and T */
      template<typename T, typename R, typename U, R(*func)(T, U)> void registerFreeBinaryFunction(const vm::opcode_t& opcode, ValueType returnType, ValueType arg1, ValueType arg2)
      {
        registerBinary(opcode, Signature(returnType, arg1, arg2), [](Value v1, Value v2) { return Value(func(v1.as<T>(), v2.as<T>())); });
      }

    public:
      const FunctionDefinition* find(const opcode_t& opcode, ValueType t1, ValueType t2 = ValueType::NONE, ValueType t3 = ValueType::NONE) const
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

      Functions() { init(); }
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

    class Environment
    {
    private:
      const Functions* functions;
      const Enums* enums;

      std::unordered_map<std::string, TypedValue> variables;
    public:
      Environment(const Functions* functions, const Enums* enums) : functions(functions), enums(enums) { }

      const FunctionDefinition* findFunction(const opcode_t& opcode, ValueType t1, ValueType t2 = ValueType::NONE, ValueType t3 = ValueType::NONE) const { return functions->find(opcode, t1, t2, t3); }
      auto findEnum(const std::string& name, const std::string& value) const { return enums->findEnum(name, value); }

      void set(const std::string& ident, TypedValue value) { variables.emplace(std::make_pair(ident, value)); }
      const TypedValue* get(const std::string& identifier) const { auto it = variables.find(identifier); return it != variables.end() ? &it->second : nullptr; }
    };
  }

  namespace ast
  {
    struct CompileResult
    {
      bool success;
      ValueType type;
      std::string message;
      vm::lambda_t lambda;

      CompileResult(ValueType type, vm::lambda_t lambda) : success(true), type(type), message(""), lambda(lambda) { }
      CompileResult(std::string&& message) : success(false), type(ValueType::NONE), message(message) { }
      CompileResult() : success(false) { }

      operator bool() const { return success; }

      static const char* nameForType(ValueType type)
      {
        switch (type) {
          case ValueType::BOOL: return "bool";
          case ValueType::INTEGRAL: return "integral";
          case ValueType::REAL: return "real";
          default: return "n/a";
        }
      }
    };

    class Expression
    {
    protected:

    public:
      virtual CompileResult compile(const vm::Environment* env) const = 0;
      virtual std::string textual(size_t indent = 0) const = 0;
      virtual bool isConstant(const vm::Environment* env) const { return false; }
    };

    class LiteralValue : public Expression
    {
    private:
      ValueType _type;
      Value _value;

    public:
      template<typename T> LiteralValue(ValueType type, T value) : _type(type), _value(value) { }
      bool isConstant(const vm::Environment* env) const override{ return true; }

      CompileResult compile(const vm::Environment*) const override
      {
        return CompileResult(_type, [value = _value]() { return value; });
      }

      std::string textual(size_t indent) const override
      {
        auto result = std::string(indent, ' ') + "value(";
        if (_type == ValueType::INTEGRAL) return result + std::to_string(_value.i()) + ")\n";
        else if (_type == ValueType::REAL) return result + std::to_string(_value.r()) + ")\n";
        else if (_type == ValueType::BOOL) return result + (_value.b() ? "true)\n" : "false)\n");
        else assert(false);
      }
    };

    class Identifier : public Expression
    {
    private:
      std::string _identifier;

    public:
      Identifier(const std::string& identifier) : _identifier(identifier) { }
      bool isConstant(const vm::Environment* env) const override { return true; } //TODO: should fetch value from env and decide


      CompileResult compile(const vm::Environment* env) const override
      {
        //TODO: here we assume that identifier is already defined in symbol table, is that correct?
        const TypedValue* value = env->get(_identifier);
        return CompileResult(value->type, [value = value->value]() { return value; });
      }

      std::string textual(size_t indent = 0) const override { return std::string(indent, ' ') + "identifier(" + _identifier + ")\n"; }
    };

    class EnumValue : public Expression
    {
    private:
      std::string _enumName;
      std::string _enumValue;

    public:
      EnumValue(const std::string& enumName, const std::string& enumValue) : _enumName(enumName), _enumValue(enumValue) { }
      bool isConstant(const vm::Environment* env) const override { return true; }

      CompileResult compile(const vm::Environment* env) const override
      {
        auto value = env->findEnum(_enumName, _enumValue);

        if (value.first)
          return CompileResult(ValueType::INTEGRAL, [value = value.second]() { return value; });

        return CompileResult("Enum value " + _enumName + "::" + _enumValue + " not found");
      }

      std::string textual(size_t indent = 0) const override { return std::string(indent, ' ') + "enum(" + _enumName + "::" + _enumValue + ")\n"; }

    };

    class FunctionCall : public Expression
    {
    private:
      vm::opcode_t _identifier;
      std::vector<std::unique_ptr<Expression>> _args;

    public:
      FunctionCall(const vm::opcode_t& identifier, const std::vector<Expression*>& args) : _identifier(identifier)
      {
        for (Expression* arg : args)
          _args.emplace_back(arg);
      }
      bool isConstant(const vm::Environment* env) const override { return true; } // TODO env.functions->find()->isConstant

      CompileResult compile(const vm::Environment* env) const override
      {
        std::vector<CompileResult> args;
        std::transform(_args.begin(), _args.end(), std::back_inserter(args), [env](const auto& expr) { return expr->compile(env); });

        auto failed = std::find_if(args.begin(), args.end(), [](const auto& result) { return !result.success; });

        if (failed != args.end())
          return *failed;
        else
        {
          auto types = std::array<ValueType, 3>({ ValueType::NONE, ValueType::NONE, ValueType::NONE });
          assert(types.size() >= args.size());

          for (size_t i = 0; i < args.size(); ++i)
            types[i] = args[i].type;

          bool retry = true;

          while (retry)
          {

            auto definition = env->findFunction(_identifier, types[0], types[1], types[2]);

            if (definition != nullptr)
            {
              const auto& functor = definition->function;
              CompileResult result;

              switch (_args.size())
              {
                case 0: result = CompileResult(definition->signature.returnType, [function = functor.nullary]() { return function(); }); break;
                case 1: result = CompileResult(definition->signature.returnType, [function = functor.unary, first = args[0].lambda]() { return function(first()); }); break;
                case 2: result = CompileResult(definition->signature.returnType, [function = functor.binary, first = args[0].lambda, second = args[1].lambda]() { return function(first(), second()); }); break;
                  //TODO: ternary
              }

              if (config::OptimizationConstantFolding && std::all_of(_args.begin(), _args.end(), [env](const auto& arg) { return arg->isConstant(env); }) && definition->isConstant)
              {
                Value value = result.lambda();
                result.lambda = [value = value] { return value; };
              }

              return result;
            }
            else
            {
              /* try with promotion to float if available */
              if (config::EnableIntegerPromotionToReal)
              {
                retry = false;
                for (size_t i = 0; i < types.size(); ++i)
                {
                  if (types[i] == ValueType::INTEGRAL)
                  {
                    args[i].lambda = [old = args[i].lambda]() { Value v = old();  return Value((real_t)v.i()); };
                    types[i] = ValueType::REAL;
                    retry = true;
                  }
                }
              }
            }
          }

          /* no function matching the signature, failure */
          return CompileResult("no matching function found for " + _identifier + " " + CompileResult::nameForType(types[0])); //TODO: finish log

        }

      }

      std::string textual(size_t indent) const override
      {
        std::string result = std::string(indent, ' ') + "call(" + _identifier + ")\n";
        for (const auto& arg : _args)
          result += arg->textual(indent + 2);
        return result;
      }

    };
  }

  namespace parser
  {
    using token_value_t = std::string;
    using namespace lex;

    struct ParserResult
    {
      std::unique_ptr<ast::Expression> ast;
      bool success;
      std::string message;
    };

    class Parser
    {
    protected:
      lex::token_list tokens;
      lex::token_list::const_iterator token;

    protected:
      bool check(TokenType type) { return !finished() && token->type() == type; }

      bool finished() const { return token == tokens.end(); }

      bool match(TokenType type)
      {
        bool matched = check(type);
        if (matched)
          advance();
        return matched;
      }

      bool match(TokenType type, const std::initializer_list<token_value_t> & tokens)
      {
        if (check(type))
        {
          const token_value_t& value = token->textual();
          bool matched = std::find(tokens.begin(), tokens.end(), value) != tokens.end();

          if (matched)
            advance();

          return matched;
        }

        return false;
      }

      auto previous() const
      {
        return std::prev(token);
      }

      bool consume(TokenType type, const std::initializer_list<token_value_t>& tokens, const std::string& error)
      {
        if (!match(type, tokens))
          return false;
        return true;
      }

      void advance() { ++token; }
      const Token& peek() const { return *token; }

    protected:
      template<ast::Expression* (Parser::*function)()>
      ast::Expression* binary(TokenType type, const std::initializer_list<token_value_t>& tokens)
      {
        ast::Expression* left = (this->*function)();

        while (match(type, tokens))
        {
          vm::opcode_t op = previous()->textual();
          ast::Expression* right = (this->*function)();
          left = new ast::FunctionCall(op, { left, right });
        }

        return left;
      }

    protected:
      ast::Expression* expression() { return condition(); }

      /* condition = equality ( [&& ||] condition )* */
      ast::Expression* condition() { return binary<&Parser::equality>(TokenType::OPERATOR, { "&&", "||" }); }

      /* equality = comparison ( [ == != ] comparison ) */
      ast::Expression* equality() { return binary<&Parser::comparison>(TokenType::OPERATOR, { "==", "!=" }); }

      /* comparison = addition ( [ >= <= > < ] addition ) */
      ast::Expression* comparison() { return binary<&Parser::addition>(TokenType::OPERATOR, { ">=", "<=", ">", "<" }); }

      /* addition = multiplication ( [+ -] multiplication )* */
      ast::Expression* addition() { return binary<&Parser::multiplication>(TokenType::OPERATOR, { "+", "-" }); }

      /* multiplication = unary ( [* / %] unary )* */
      ast::Expression* multiplication() { return binary<&Parser::unary>(TokenType::OPERATOR, { "*", "/", "%" }); }

      /* unary = ( [- ~ !] primary )*/
      ast::Expression* unary()
      {
        if (match(TokenType::OPERATOR, { "!", "~", "-" }))
        {
          const token_value_t& op = previous()->textual();
          ast::Expression* expr = primary();
          return new ast::FunctionCall(op, { expr });
        }

        return primary();
      }

      /* primary ::= BOOL | INT | FLOAT | '(' expression ')' | IDENTIFIER | IDENTIFIER '(' ( expression ',' ) * ')' */
      ast::Expression* primary()
      {
        if (match(TokenType::BOOLEAN))
          return new ast::LiteralValue(ValueType::BOOL, previous()->value());
        else if (match(TokenType::INTEGRAL))
          return new ast::LiteralValue(ValueType::INTEGRAL, previous()->value());
        else if (match(TokenType::FLOAT))
          return new ast::LiteralValue(ValueType::REAL, previous()->value());
        else if (match(TokenType::SYMBOL, { "(" }))
        {
          ast::Expression* expr = expression();
          consume(TokenType::SYMBOL, { ")" }, "expecting ')' after expression");
          return expr;
        }
        else if (match(TokenType::IDENTIFIER))
        {
          /* it's a function call */
          if (!finished() && peek().match(TokenType::SYMBOL, "("))
          {
            std::string identifier = previous()->textual();

            advance();
            std::vector<ast::Expression*> arguments;
            bool done = false;

            /* comma separated expressions as arguments */

            while (!done)
            {
              if (finished())
                done = true; /*TODO: unexpected eof while parsing function arguments */
              else if (match(TokenType::SYMBOL, { ")" }))
                return new ast::FunctionCall(identifier, arguments);
              else if (match(TokenType::SYMBOL, { "," }) && !arguments.empty())
                ;
              else
                arguments.push_back(expression());
            }
          }
          /* it's an enum reference */
          else if (!finished() && peek().match(TokenType::SYMBOL, "::"))
          {
            std::string enumName = previous()->textual();
            advance();

            if (finished() || !match(TokenType::IDENTIFIER))
              return fail("expecting an identifier after enum name");
            else
            {
              std::string enumValue = previous()->textual();
              return new ast::EnumValue(enumName, enumValue);
            }
          }
          /* it's a raw identifier*/
          else
          {
            return new ast::Identifier(previous()->textual());
          }

        }

        return nullptr;
      }

      ast::Expression* fail(const std::string& message) { return nullptr; }

    public:
      explicit Parser() { }

      ParserResult parse(const lex::token_list& tokens)
      { 
        this->tokens = tokens;
        this->token = this->tokens.begin();
        auto* ast = expression();
        return { std::unique_ptr<ast::Expression>(ast), ast != nullptr, "" };
      }
    };
  }


}

void nanoexpr::vm::Functions::init()
{
  using V = Value;
  using VT = ValueType;

  /* numerical */
  registerNumericBinary<false, std::plus>("+");
  registerNumericBinary<false, std::minus>("-");
  registerNumericBinary<false, std::multiplies>("*");
  registerNumericBinary<false, std::divides>("/");
  registerBinary("%", vm::Signature(VT::INTEGRAL, VT::INTEGRAL, VT::INTEGRAL), [](V v1, V v2) { return std::modulus<bool>()(v1.i(), v2.i()); });
  registerNumericUnary<false, std::negate>("-");

  /* bitwise */
  registerUnary("~", vm::Signature(VT::INTEGRAL, VT::INTEGRAL), [](V v) { return std::bit_not<bool>()(v.i()); });
  registerBinary("&", vm::Signature(VT::INTEGRAL, VT::INTEGRAL, VT::INTEGRAL), [](V v1, V v2) { return std::bit_and<bool>()(v1.b(), v2.b()); });
  registerBinary("|", vm::Signature(VT::INTEGRAL, VT::INTEGRAL, VT::INTEGRAL), [](V v1, V v2) { return std::bit_or<bool>()(v1.b(), v2.b()); });

  /* equality */
  registerNumericBinary<true, std::equal_to>("==");
  registerNumericBinary<true, std::not_equal_to>("!=");

  /* comparison */
  registerNumericBinary<true, std::less>("<");
  registerNumericBinary<true, std::less_equal>("<=");
  registerNumericBinary<true, std::greater>(">");
  registerNumericBinary<true, std::greater_equal>("<=");

  /* logical operators */
  registerBinary("&&", vm::Signature(VT::BOOL, VT::BOOL, VT::BOOL), [](V v1, V v2) { return std::logical_and<bool>()(v1.b(), v2.b()); });
  registerBinary("||", vm::Signature(VT::BOOL, VT::BOOL, VT::BOOL), [](V v1, V v2) { return std::logical_or<bool>()(v1.b(), v2.b()); });
  registerBinary("==", vm::Signature(VT::BOOL, VT::BOOL, VT::BOOL), [](V v1, V v2) { return std::equal_to<bool>()(v1.b(), v2.b()); });
  registerBinary("!=", vm::Signature(VT::BOOL, VT::BOOL, VT::BOOL), [](V v1, V v2) { return std::not_equal_to<bool>()(v1.b(), v2.b()); });
  registerUnary("!", vm::Signature(VT::BOOL, VT::BOOL), [](V v) { return std::logical_not<bool>()(v.b()); });

  /* builtins */
  registerBinary("min", { ValueType::INTEGRAL, ValueType::INTEGRAL, ValueType::INTEGRAL }, [](V v1, V v2) { return std::min(v1.i(), v2.i()); });
  registerBinary("min", { ValueType::REAL, ValueType::REAL, ValueType::REAL }, [](V v1, V v2) { return std::min(v1.r(), v2.r()); });
  registerBinary("max", { ValueType::INTEGRAL, ValueType::INTEGRAL, ValueType::INTEGRAL }, [](V v1, V v2) { return std::max(v1.i(), v2.i()); });
  registerBinary("max", { ValueType::REAL, ValueType::REAL, ValueType::REAL }, [](V v1, V v2) { return std::max(v1.r(), v2.r()); });

  registerFreeUnaryFunction<real_t, real_t, std::abs>("abs", ValueType::REAL, ValueType::REAL);
  registerFreeUnaryFunction<integral_t, integral_t, std::abs>("abs", ValueType::INTEGRAL, ValueType::INTEGRAL);
  registerFreeUnaryFunction<real_t, real_t, std::sqrt>("sqrt", ValueType::REAL, ValueType::REAL);

  if (nanoexpr::config::EnableTrigonometricFunctions)
  {
    registerFreeUnaryFunction<real_t, real_t, std::cos>("cos", ValueType::REAL, ValueType::REAL);
    registerFreeUnaryFunction<real_t, real_t, std::sin>("sin", ValueType::REAL, ValueType::REAL);
    registerFreeUnaryFunction<real_t, real_t, std::tan>("tan", ValueType::REAL, ValueType::REAL);
    registerFreeUnaryFunction<real_t, real_t, std::acos>("acos", ValueType::REAL, ValueType::REAL);
    registerFreeUnaryFunction<real_t, real_t, std::acos>("asin", ValueType::REAL, ValueType::REAL);
    registerFreeUnaryFunction<real_t, real_t, std::atan>("atan", ValueType::REAL, ValueType::REAL);
    registerFreeBinaryFunction<real_t, real_t, real_t, std::atan2>("atan", ValueType::REAL, ValueType::REAL, ValueType::REAL);
  }

  if (nanoexpr::config::EnableHyperbolicFunctions)
  {
    registerFreeUnaryFunction<real_t, real_t, std::cosh>("cosh", ValueType::REAL, ValueType::REAL);
    registerFreeUnaryFunction<real_t, real_t, std::sinh>("sinh", ValueType::REAL, ValueType::REAL);
    registerFreeUnaryFunction<real_t, real_t, std::tanh>("tanh", ValueType::REAL, ValueType::REAL);
    registerFreeUnaryFunction<real_t, real_t, std::acosh>("acosh", ValueType::REAL, ValueType::REAL);
    registerFreeUnaryFunction<real_t, real_t, std::acosh>("asinh", ValueType::REAL, ValueType::REAL);
    registerFreeUnaryFunction<real_t, real_t, std::atanh>("atanh", ValueType::REAL, ValueType::REAL);
  }
}

nanoexpr::lex::Lexer::Lexer()
{
  rules.emplace_back(new WhiteSpaceRule());
  rules.emplace_back(new BooleanRule());
  rules.emplace_back(new FloatingRule());
  rules.emplace_back(new IntegerRule());
  rules.emplace_back(new IdentifierRule());
  rules.emplace_back(new OperatorRule(TokenType::OPERATOR, false,
                                      {
                                        "+", "-", "*", "/", "%",
                                        "||", "&&", "!", "~",
                                        "==", "!=", ">=", "<=", ">", "<"
                                      }
  ));
  rules.emplace_back(new OperatorRule(TokenType::SYMBOL, false, { "(", ")", ",", "::"/*, ":", "?" */ }));
}

nanoexpr::lex::LexerResult nanoexpr::lex::Lexer::parse(const std::string& text)
{
  std::vector<Token> tokens;

  size_t p = 0;

  while (p < text.length())
  {
    bool foundAny = false;
    std::string_view input = std::string_view(text).substr(p);

    for (const auto& rule : rules)
    {
      auto token = rule->matches(input);

      /* a match has been found for this rule */
      if (token.valid())
      {
        if (token.type() != TokenType::SKIP)
          tokens.push_back(token);

        p += token.length();
        foundAny = true;
        break;
      }
    }

    if (!foundAny)
    {
      auto end = text.find_first_of(" \t\n\r", p);
      return { {}, false, std::string("unknown token: ") + text.substr(p, end - p) };
    }
  }

  return { tokens, true, "" };
}