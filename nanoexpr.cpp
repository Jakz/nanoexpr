#include <cstdlib>
#include <cstdio>
#include <iostream>
#include <cassert>

#include <cstdint>
#include <unordered_map>
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
  using real_t = float;
  using integral_t = int32_t;
  
  using operator_enum_t = uint64_t;

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

  template<ValueType T> struct value_traits{ };
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

    integral_t i() const { return integral; }
    real_t r() const { return real; }
    bool b() const { return boolean; }
    template<typename T> T* p() const { return static_cast<T*>(ptr); }

    template<typename T> T as();
    template<ValueType T> typename value_traits<T>::type as() { return as<typename value_traits<T>::type>(); }
  };

  template<> real_t Value::as() { return real; }
  template<> integral_t Value::as() { return integral; }
  template<> bool Value::as() { return real; }

  struct TypedValue
  {
    Value value;
    ValueType type;

    TypedValue(real_t real) : value(real), type(ValueType::REAL) { }
    TypedValue(integral_t real) : value(real), type(ValueType::INTEGRAL) { }
    TypedValue(bool real) : value(real), type(ValueType::BOOL) { }
  };

  enum class TokenType
  {
    NONE,
    SKIP,

    IDENTIFIER,
    /*STRING,*/
    FLOAT_LITERAL,
    INTEGRAL_LITERAL,
    BOOLEAN_LITERAL,

    OPERATOR,
    SYMBOL
  };

  enum class Operator
  {
    LOGICAL_AND, LOGICAL_OR, LOGICAL_NOT,
    
    EQ, NEQ,
    GEQ, GRE, LEQ, LES,

    AND, OR, XOR, NOT,

    LSHIFT, RSHIFT,

    PLUS, MINUS, 
    DIVIDE, MULTIPLY, MODULUS, EXP,    
  };

  enum class Symbol
  {
    LPAREN,
    RPAREN,
  };

  class Token
  {
    TokenType _type;
    std::string _textual;

    union
    {
      Value _value;
      operator_enum_t _token;
    };

  public:
    Token(TokenType type) : _type(type) { }
    Token(TokenType type, std::string_view textual) : _type(type), _textual(textual) { }
    template<typename T, typename std::enable_if<!std::is_enum<T>::value>::type* = nullptr> Token(TokenType type, std::string_view textual, T value) : _type(type), _textual(textual), _value(value) { }
    template<typename T, typename std::enable_if<std::is_enum<T>::value>::type* = nullptr> Token(TokenType type, std::string_view textual, T value) : _type(type), _textual(textual), _token(static_cast<operator_enum_t>(value)) { }



    TokenType type() const { return _type; }
    size_t length() const { return _textual.length(); }
    const Value& value() const { return _value;  }
    template<typename T> T token() const { return static_cast<T>(_token);  }
    const auto& textual() const { return _textual; }

    bool valid() const { return _type != TokenType::NONE; }

    template<typename T> bool match(TokenType type, T token) const { return _type == type && static_cast<operator_enum_t>(token) == _token; }
  };

  namespace lex
  {
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
        return input.length() == position || std::isspace(input[position]) || !std::isdigit(input[position]);
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

    class IntegerRule : public Rule
    {
    public:
      Token matches(const std::string_view input) const override
      {
        bool isNegative = false;
        size_t p = 0;

        if (input[p] == '-')
        {
          isNegative = true;
          ++p;
        }

        while (p < input.length() && std::isdigit(input[p]))
          ++p;

        if (p > 0 && (!isNegative || p > 1) && Rule::hasSpaceOrNonDigitTermination(input, p))
        {
          std::string copy = std::string(input.substr(0, p));
          return Token(TokenType::INTEGRAL_LITERAL, input.substr(0, p), std::stoi(copy)); //TODO: stoi, choose according to integer_t type?
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
          return Token(TokenType::BOOLEAN_LITERAL, "false", false);
        else if (Rule::matches(input, "true"))
          return Token(TokenType::BOOLEAN_LITERAL, "true", true);
        else
          return TokenType::NONE;
      }
    };

    template<typename T>
    class OperatorRule : public Rule
    {
      using mapping_t = std::unordered_map<std::string, T>;

    private:
      bool _requireSpacing;
      TokenType _type;
      mapping_t _mapping;

    public:
      OperatorRule(TokenType type, bool requireSpacing, const std::initializer_list<std::pair<T, std::string>>& mapping) : _type(type), _requireSpacing(requireSpacing)
      {
        std::transform(mapping.begin(), mapping.end(), std::inserter(_mapping, _mapping.end()), 
                       [](const auto& pair) { return std::make_pair(pair.second, pair.first); 
        });
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
              return Token(_type, token, it->second);
          }
        }
        else
        {
          for (const typename mapping_t::value_type& entry : _mapping)
          {
            if (input.length() >= entry.first.length())
            {
              if (input.substr(0, entry.first.length()) == entry.first)
                return Token(_type, entry.first, entry.second);
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

    using Opcode = uint64_t;

    struct VariantFunctor
    {
      size_t args;

      union
      {
        unary_operation unary;
        binary_operation binary;
      };

      VariantFunctor(unary_operation unary) : args(1), unary(unary) { }
      VariantFunctor(binary_operation binary) : args(2), binary(binary) { }
      VariantFunctor(const VariantFunctor& o) { this->operator=(o); }

      VariantFunctor& operator=(const VariantFunctor& o)
      {
        this->args = o.args;

        switch (args) {
          case 1: new (&unary) unary_operation(); this->unary = o.unary; break;
          case 2: new (&binary) binary_operation(); this->binary = o.binary; break;
        }

        return *this;
      }

      ~VariantFunctor()
      {
        switch (args) {
          case 1: unary.~unary_operation(); break;
          case 2: binary.~binary_operation(); break;
        }
      }
    };

    class Functions
    {
    public:
      std::unordered_map<Opcode, std::vector<std::pair<Signature, VariantFunctor>>> functors;

      template<typename T> void registerBinary(T opcode, Signature signature, binary_operation functor) { functors[static_cast<Opcode>(opcode)].push_back(std::make_pair(signature, functor)); }
      template<typename T> void registerUnary(T opcode, Signature signature, unary_operation functor) { functors[static_cast<Opcode>(opcode)].push_back(std::make_pair(signature, functor)); }


      template<typename T, bool Cmp, template<typename TT> typename F> void registerNumericBinary(T opcode)
      {
        registerBinary(opcode, Signature(Cmp ? ValueType::BOOL : ValueType::INTEGRAL, ValueType::INTEGRAL, ValueType::INTEGRAL), [](Value v1, Value v2) { return Value(F<integral_t>()(v1.i(), v2.i())); });
        registerBinary(opcode, Signature(Cmp ? ValueType::BOOL : ValueType::REAL, ValueType::REAL, ValueType::REAL), [](Value v1, Value v2) { return Value(F<real_t>()(v1.r(), v2.r())); });
      }

    public:
      template<typename T> std::pair<ValueType, const VariantFunctor*> find(T opcode, ValueType t1, ValueType t2 = ValueType::NONE) const
      {
        const auto& functorsByOpcode = functors.find(static_cast<Opcode>(opcode));

        if (functorsByOpcode != functors.end())
        {
          Signature actual = Signature(ValueType::NONE, t1, t2);
          auto it = std::find_if(functorsByOpcode->second.begin(), functorsByOpcode->second.end(), [&actual](const auto& pair) { return pair.first == actual; });
          
          if (it != functorsByOpcode->second.end())
            return std::make_pair(it->first.returnType, &it->second);
        }

        return std::make_pair(ValueType::NONE, nullptr);
      }

    public:
      Functions()
      {
        using V = Value;
        using VT = ValueType;
        
        /* numerical */
        registerNumericBinary<Operator, false, std::plus>(Operator::PLUS);
        registerNumericBinary<Operator, false, std::minus>(Operator::MINUS);
        registerNumericBinary<Operator, false, std::multiplies>(Operator::MULTIPLY);
        registerNumericBinary<Operator, false, std::divides>(Operator::DIVIDE);

        /* equality */
        registerNumericBinary<Operator, true, std::equal_to>(Operator::EQ);
        registerNumericBinary<Operator, true, std::not_equal_to>(Operator::NEQ);

        /* comparison */
        registerNumericBinary<Operator, true, std::less>(Operator::LES);
        registerNumericBinary<Operator, true, std::less_equal>(Operator::LEQ);
        registerNumericBinary<Operator, true, std::greater>(Operator::GRE);
        registerNumericBinary<Operator, true, std::greater_equal>(Operator::LEQ);

        /* logical operators */
        registerBinary(Operator::LOGICAL_AND, Signature(VT::BOOL, VT::BOOL, VT::BOOL), [](V v1, V v2) { return std::logical_and<bool>()(v1.b(), v2.b()); });
        registerBinary(Operator::LOGICAL_OR, Signature(VT::BOOL, VT::BOOL, VT::BOOL), [](V v1, V v2) { return std::logical_or<bool>()(v1.b(), v2.b()); });
        registerBinary(Operator::EQ, Signature(VT::BOOL, VT::BOOL, VT::BOOL), [](V v1, V v2) { return std::equal_to<bool>()(v1.b(), v2.b()); });
        registerBinary(Operator::NEQ, Signature(VT::BOOL, VT::BOOL, VT::BOOL), [](V v1, V v2) { return std::not_equal_to<bool>()(v1.b(), v2.b()); });
        registerUnary(Operator::LOGICAL_NOT, Signature(VT::BOOL, VT::BOOL), [](V v) { return std::logical_not<bool>()(v.b()); });      }
    };

    class Envinronment
    {
    private:
      std::unordered_map<std::string, TypedValue> variables;
    public:
      Functions functions;

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
    
    class Node 
    { 
    public:
      virtual CompileResult compile(const vm::Envinronment* env) const = 0;
    };

    class Expression : public Node
    { 
    public:
    };

    class LiteralValue : public Expression
    {
    private:
      ValueType _type;
      Value _value;

    public:
      template<typename T> LiteralValue(ValueType type, T value) : _type(type), _value(value) { }

      CompileResult compile(const vm::Envinronment*) const override 
      { 
        return CompileResult(_type, [value = _value]() { return value; });
      }

    };

    class Identifier : public Expression
    {
    private:
      std::string _identifier;

    public:
      Identifier(const std::string& identifier) : _identifier(identifier) { }

      CompileResult compile(const vm::Envinronment* env) const override
      { 
        //TODO: here we assume that identifier is already defined in symbol table, is that correct?
        const TypedValue* value = env->get(_identifier); 
        return CompileResult(value->type, [value = value->value]() { return value; }); 
      }
    };

    class UnaryExpression : public Expression
    {
    private:
      Operator _op;
      std::unique_ptr<Expression> _expr;

    public:
      UnaryExpression(Operator op, Expression* expr) : _op(op), _expr(expr) { }

      CompileResult compile(const vm::Envinronment* env) const override
      {
        auto expr = _expr->compile(env);

        if (!expr) return expr;
        else
        {
          auto type = expr.type;
          bool retry = true;

          while (retry)
          {
            auto entry = env->functions.find(_op, type);
            const auto* functor = entry.second;

            if (functor)
            {
              assert(functor->args == 1);
              return CompileResult(entry.first, [function = functor->unary, lambda = expr.lambda]() { return function(lambda()); });
            }

            /* try with promotion to other integral type */
            //TODO: maybe one way only int -> real
            retry = false;
            if (type == ValueType::INTEGRAL)
            {
              type = ValueType::REAL;
              expr.lambda = [old = expr.lambda]() { Value v = old(); return Value((real_t)v.i()); };
              retry = true;
            }
          }

          /* no function matching the signature, failure */
          return CompileResult("no matching function found for " + std::to_string((int)_op) + " " + CompileResult::nameForType(type)); //TODO finish message
        }
      }
    };

    class BinaryExpression : public Expression
    {
    private:
      Operator _op;
      std::unique_ptr<Expression> _left, _right;

    public:
      BinaryExpression(Operator op, Expression* left, Expression* right) : _op(op), _left(left), _right(right) { }

      CompileResult compile(const vm::Envinronment* env) const override
      {
        auto left = _left->compile(env), right = _right->compile(env);

        if (!left) return left;
        else if (!right) return right;
        else
        {
          auto leftType = left.type, rightType = right.type;
          bool retry = true;

          while (retry)
          {
            auto entry = env->functions.find(_op, leftType, rightType);
            const auto* functor = entry.second;

            if (functor)
            {
              assert(functor->args == 2);
              return CompileResult(entry.first, [function = functor->binary, left = left.lambda, right = right.lambda]() { return function(left(), right()); });
            }

            /* try with promotion to float if available */
            retry = false;
            if (leftType == ValueType::INTEGRAL)
            {
              leftType = ValueType::REAL;
              left.lambda = [old = left.lambda] () { Value v = old(); return Value((real_t)v.i()); };
              retry = true;
            }
            if (rightType == ValueType::INTEGRAL)
            {
              rightType = ValueType::REAL; 
              right.lambda = [old = right.lambda]() { Value v = old(); return Value((real_t)v.i()); };
              retry = true; 
            }
          }

          /* no function matching the signature, failure */
          return CompileResult("no matching function found for " + std::to_string((int)_op) + " " + CompileResult::nameForType(left.type) + ", " + CompileResult::nameForType(right.type)); //TODO finish message
        }
      }
    };
    
    class FunctionCall : public Expression
    {
    private:
      std::string _identifier;
      std::vector<std::unique_ptr<Expression>> _args;
      
    public:
      FunctionCall(const std::string& identifier, const std::vector<Expression*>& args) : _identifier(identifier), _args(args.size())
      {
        for (Expression* arg : args)
          _args.emplace_back(arg);
      }
      
      CompileResult compile(const vm::Envinronment* env) const override
      {
        std::vector<CompileResult> args;
        std::transform(_args.begin(), _args.end(), std::back_inserter(args), [env] ( const auto& expr) { return expr->compile(env); });
        
        auto failed = std::find_if(args.begin(), args.end(), []( const auto& result ) { return !result.success; });
        
        if (failed != args.end())
          return *failed;
        else
        {
          
        }
        
        return CompileResult("fail");
      }
    };
  }
 
  namespace parser
  {
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

      template<typename T, typename std::enable_if<std::is_enum<T>::value && !std::is_same<T, TokenType>::value, T>::type* = nullptr> 
      bool match(TokenType type, const std::initializer_list<T>& tokens)
      {
        if (check(type))
        {
          T value = token->token<T>();
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

      template<typename T, typename std::enable_if<std::is_enum<T>::value && !std::is_same<T, TokenType>::value, T>::type* = nullptr> 
      T previous()
      {
        return std::prev(token)->token<T>();
      }

      template<typename T, typename std::enable_if<std::is_enum<T>::value && !std::is_same<T, TokenType>::value, T>::type* = nullptr> 
      bool consume(TokenType type, const std::initializer_list<T>& tokens, std::string error)
      {
        if (!match(type, tokens))
          return false;
        return true;
      }

      void advance() { ++token;  }
      const Token& peek() const { return *token;  }

    protected:
      template<ast::Expression* (Parser::*function)()>
      ast::Expression* binary(TokenType type, const std::initializer_list<Operator>& tokens)
      {
        ast::Expression* left = (this->*function)();

        while (match(type, tokens))
        {
          Operator op = previous<Operator>();
          ast::Expression* right = (this->*function)();
          left = new ast::BinaryExpression(op, left, right);
        }

        return left;
      }

    protected:
      ast::Expression* expression() { return condition(); }

      /* condition = equality ( [&& ||] condition )* */
      ast::Expression* condition() { return binary<&Parser::equality>(TokenType::OPERATOR, { Operator::LOGICAL_AND, Operator::LOGICAL_OR }); }

      /* equality = comparison ( [ == != ] comparison ) */
      ast::Expression* equality() { return binary<&Parser::comparison>(TokenType::OPERATOR, { Operator::EQ, Operator::NEQ }); }

      /* comparison = addition ( [ >= <= > < ] addition ) */
      ast::Expression* comparison() { return binary<&Parser::addition>(TokenType::OPERATOR, { Operator::GEQ, Operator::GRE, Operator::LEQ, Operator::LES }); }

      /* addition = multiplication ( [+ -] multiplication )* */
      ast::Expression* addition() { return binary<&Parser::multiplication>(TokenType::OPERATOR, { Operator::PLUS, Operator::MINUS }); }

      /* multiplication = unary ( [* / %] unary )* */
      ast::Expression* multiplication() { return binary<&Parser::unary>(TokenType::OPERATOR, { Operator::MULTIPLY, Operator::DIVIDE, Operator::MODULUS }); }

      /* unary = ( [~ !] primary )*/
      ast::Expression* unary()
      { 
        if (match(TokenType::OPERATOR, { Operator::LOGICAL_NOT }))
        {
          Operator op = previous<Operator>();
          ast::Expression* expr = primary();
          return new ast::UnaryExpression(op, expr);
        }

        return primary();
      }

      /* primary ::= BOOL | INT | FLOAT | '(' expression ')' | IDENTIFIER | IDENTIFIER '(' ( expression ',' ) * ')' */
      ast::Expression* primary()
      {
        if (match(TokenType::BOOLEAN_LITERAL))
          return new ast::LiteralValue(ValueType::BOOL, previous()->value());
        else if (match(TokenType::INTEGRAL_LITERAL))
          return new ast::LiteralValue(ValueType::INTEGRAL, previous()->value());
        else if (match(TokenType::FLOAT_LITERAL))
          return new ast::LiteralValue(ValueType::REAL, previous()->value());
        else if (match(TokenType::SYMBOL, { Symbol::LPAREN }))
        {
          ast::Expression* expr = expression();
          consume(TokenType::SYMBOL, { Symbol::RPAREN }, "expecting ')' after expression");
          return expr;
        }
        else if (match(TokenType::IDENTIFIER))
        {
          /* it's a function call */
          if (!finished() && peek().match(TokenType::SYMBOL, Symbol::LPAREN))
          {
            std::string identifier = previous()->textual();
            
            advance();
            std::vector<ast::Expression*> arguments;
            
            /* comma separated expressions as arguments */
            
            /* arguments ended */
            if (!finished() & peek().match(TokenType::SYMBOL, Symbol::RPAREN))
              return new ast::FunctionCall(identifier, arguments);
          }
          /* it's a raw identifier*/
          else
          {
            return new ast::Identifier(previous()->textual());
          }
            
        }

        return nullptr;
      }

    public:
      Parser(const lex::token_list& tokens) : tokens(tokens), token(this->tokens.begin()) { }

      ast::Node* parse() { return expression(); }
    };
  }


}

nanoexpr::lex::Lexer::Lexer()
{
  rules.emplace_back(new WhiteSpaceRule());
  rules.emplace_back(new BooleanRule());
  rules.emplace_back(new IntegerRule());
  rules.emplace_back(new IdentifierRule());
  rules.emplace_back(new OperatorRule<Operator>(
    TokenType::OPERATOR, false,
    { 
      { Operator::PLUS, "+" }, { Operator::MINUS, "-" },
      { Operator::MULTIPLY, "*" }, { Operator::DIVIDE, "/"}, { Operator::MODULUS, "%" },
      { Operator::LOGICAL_OR, "||"}, { Operator::LOGICAL_AND, "&&" }, { Operator::LOGICAL_NOT, "!" },
      { Operator::EQ, "==" }, { Operator::NEQ, "!=" },
      { Operator::GEQ, ">="}, { Operator::LEQ, "<=" }, { Operator::GRE, ">" }, {Operator::LES, "<" } 
    }));
  rules.emplace_back(new OperatorRule<Symbol>(
    TokenType::SYMBOL, false,
    {
      { Symbol::LPAREN, "(" },{ Symbol::RPAREN, ")" },
    }));
}

nanoexpr::lex::LexerResult nanoexpr::lex::Lexer::parse(const std::string& text)
{
  std::vector<Token> tokens;

  size_t p = 0;
  bool foundAny = false;

  while (p < text.length())
  {
    foundAny = false;
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


using namespace nanoexpr;
std::ostream& operator<<(std::ostream& out, const Token& token)
{
  switch (token.type()) {
    case TokenType::BOOLEAN_LITERAL: out << "bool(" << (token.value().boolean ? "true" : "false") << ")"; break;
    case TokenType::FLOAT_LITERAL: out << "float(" << token.value().real << ")"; break;
    case TokenType::INTEGRAL_LITERAL: out << "int(" << token.value().integral << ")"; break;
    case TokenType::OPERATOR: out << "operator('" << token.textual() << "')"; break;
    case TokenType::SYMBOL: out << "symbol('" << token.textual() << "')"; break;
    case TokenType::IDENTIFIER: out << "identifier(" << token.textual() << ")"; break;
  }

  return out;
}

int main()
{
  auto input = "!false && !true == !(false || true)";
  bool execute = true;

  nanoexpr::lex::Lexer lexer;
  auto result = lexer.parse(input);

  for (const auto& token : result.tokens)
    std::cout << token << std::endl;

  if (!result.success)
    std::cout << "lexer error: " << result.message << std::endl;
  else if (execute)
  {    
    nanoexpr::parser::Parser parser(result.tokens);
    auto ast =  parser.parse();

    vm::Envinronment env;

    env.set("x", 4.28f);

    auto result = ast->compile(&env);

    if (result)
    {
      Value v = result.lambda();

      std::cout << std::endl << input << " -> ";

      switch (result.type)
      {
        case ValueType::INTEGRAL: std::cout << v.i() << std::endl; break;
        case ValueType::REAL: std::cout << v.r() << std::endl; break;
        case ValueType::BOOL: std::cout << (v.b() ? "true" : "false") << std::endl; break;
      }
    }
    else
      std::cout << "compiler error: " << result.message << std::endl;
  }

  getchar();

  return 0;
}
