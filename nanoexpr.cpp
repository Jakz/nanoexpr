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

    NONE
  };

  template<ValueType T> struct value_traits{ };
  template<> struct value_traits<ValueType::REAL> { using type = real_t; };
  template<> struct value_traits<ValueType::INTEGRAL> { using type = integral_t; };
  template<> struct value_traits<ValueType::BOOL> { using type = bool; };

  union Value
  {
    real_t real;
    integral_t integral;
    bool boolean;

    Value(integral_t integral) : integral(integral) { }
    Value(real_t real) : real(real) { }
    Value(bool boolean) : boolean(boolean) { }

    integral_t i() const { return integral; }
    real_t r() const { return real; }
    bool b() const { return boolean; }

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
    STRING,
    FLOAT_LITERAL,
    INTEGRAL_LITERAL,
    BOOLEAN_LITERAL,

    OPERATOR,
    SYMBOL
  };

  enum class Operator
  {
    LOGICAL_AND,
    LOGICAL_OR,
    
    EQ,
    NEQ,

    GEQ,
    GRE,
    LEQ,
    LES,

    PLUS,
    MINUS,
    DIVIDE,

    MULTIPLY,
    MODULUS,
    EXP,
    
  };

  enum class Symbol
  {
    LPAREN,
    RPAREN
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
        return input.length() == position || std::isspace(input[position]) || std::isdigit(input[position]);
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
          for (const mapping_t::value_type& entry : _mapping)
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

    class Symbols
    {
    public:
      std::unordered_map<Opcode, std::vector<std::pair<Signature, VariantFunctor>>> functors;

      template<typename T> void registerBinary(T opcode, Signature signature, binary_operation functor)
      {
        functors[static_cast<Opcode>(opcode)].push_back(std::make_pair(signature, functor));
      }

    public:
      template<typename T> std::pair<ValueType, const VariantFunctor*> find(T opcode, ValueType t1, ValueType t2) const
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
      Symbols()
      {
        using V = Value;
        using VT = ValueType;
        
        registerBinary(Operator::PLUS, Signature(VT::INTEGRAL, VT::INTEGRAL, VT::INTEGRAL), [](V v1, V v2) { return V(v1.i() + v2.i()); });

        registerBinary(Operator::LES, Signature(VT::BOOL, VT::INTEGRAL, VT::INTEGRAL), [](V v1, V v2) { return V(v1.i() < v2.i()); });
        registerBinary(Operator::LES, Signature(VT::BOOL, VT::REAL, VT::INTEGRAL), [](V v1, V v2) { return V(v1.r() < v2.i()); });
        registerBinary(Operator::LES, Signature(VT::BOOL, VT::INTEGRAL, VT::REAL), [](V v1, V v2) { return V(v1.i() < v2.r()); });
        registerBinary(Operator::LES, Signature(VT::BOOL, VT::REAL, VT::REAL), [](V v1, V v2) { return V(v1.r() < v2.r()); });

      }
    };

    class Envinronment
    {
    private:
      std::unordered_map<std::string, TypedValue> variables;
    public:
      Symbols symbols;

      void set(const std::string& ident, TypedValue value) { variables.emplace(std::make_pair(ident, value)); }

      const TypedValue* get(const std::string& identifier) const { auto it = variables.find(identifier); return it != variables.end() ? &it->second : nullptr; }
    };
  }

  namespace ast
  {
    class Node 
    { 
    public:
      virtual ValueType type(const vm::Envinronment*) const = 0;
      virtual vm::lambda_t compile(const vm::Envinronment*) const = 0;
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
      ValueType type(const vm::Envinronment*) const override { return _type; }
      const auto& value() const { return _value; }

      vm::lambda_t compile(const vm::Envinronment*) const override { return [value = _value]() { return value; }; }

    };

    class Identifier : public Expression
    {
    private:
      std::string _identifier;

    public:
      Identifier(const std::string& identifier) : _identifier(identifier) { }

      ValueType type(const vm::Envinronment* env) const override { return env->get(_identifier)->type; }
      vm::lambda_t compile(const vm::Envinronment* env) const override { return [env = env, id = _identifier]() { return env->get(id)->value; }; }
    };

    class BinaryExpression : public Expression
    {
    private:
      Operator _op;
      std::unique_ptr<Expression> _left, _right;

      mutable const vm::VariantFunctor* _functor;

    public:
      BinaryExpression(Operator op, Expression* left, Expression* right) : _op(op), _left(left), _right(right), _functor(nullptr){ }
      
      ValueType type(const vm::Envinronment* env) const override
      {
        auto entry = env->symbols.find(_op, _left->type(env), _right->type(env));
        _functor = entry.second;
        assert(_functor && _functor->args == 2);
        return entry.first;
      }

      const auto& left() const { return _left; }
      const auto& right() const { return _right; }
      Operator op() { return _op; }

      vm::lambda_t compile(const vm::Envinronment* env) const override
      {        
        return[function = _functor->binary, left = left()->compile(env), right = right()->compile(env)]() { return function(left(), right()); };
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
      ast::Expression* expression() { return equality(); }

      /* equality = comparison ( [ == != ] comparison ) */
      ast::Expression* equality() { return binary<&Parser::comparison>(TokenType::OPERATOR, { Operator::EQ, Operator::NEQ }); }

      /* comparison = addition ( [ >= <= > < ] addition ) */
      ast::Expression* comparison() { return binary<&Parser::addition>(TokenType::OPERATOR, { Operator::GEQ, Operator::GRE, Operator::LEQ, Operator::LES }); }

      /* addition = multiplication ( [+ -] multiplication )* */
      ast::Expression* addition() { return binary<&Parser::multiplication>(TokenType::OPERATOR, { Operator::PLUS, Operator::MINUS }); }

      /* multiplication = unary ( [* / %] unary )* */
      ast::Expression* multiplication() { return binary<&Parser::primary>(TokenType::OPERATOR, { Operator::MULTIPLY, Operator::DIVIDE, Operator::MODULUS }); }

      /* primary ::= BOOL | INT | FLOAT | '(' expression ')' | IDENTIFIER '(' ( expression ',' ) * ')' */
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
          if (peek().match(TokenType::SYMBOL, Symbol::LPAREN))
          {

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
      { Operator::LOGICAL_OR, "||"}, { Operator::LOGICAL_AND, "&&" },
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
  auto input = "x + 5";
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

    env.set("x", 3);

    ValueType type = ast->type(&env);
    auto lambda = ast->compile(&env);

    Value v = lambda();

    std::cout << std::endl << input << " -> ";

    switch (type)
    {
      case ValueType::INTEGRAL: std::cout << v.i() << std::endl; break;
      case ValueType::REAL: std::cout << v.r() << std::endl; break;
      case ValueType::BOOL: std::cout << (v.b() ? "true" : "false") << std::endl; break;
    }
  }

  getchar();

  return 0;
}