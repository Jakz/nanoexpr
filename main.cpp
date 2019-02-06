#include <cstdlib>
#include <cstdio>
#include <iostream>

#include <cstdint>
#include <unordered_map>
#include <string>
#include <string_view>
#include <regex>
#include <vector>
#include <memory>
#include <functional>
#include <cctype>

namespace nanoexpr
{
  using real_t = float;
  using integral_t = int32_t;
  
  using operator_enum_t = uint64_t;

  union Value
  {
    real_t real;
    integral_t integral;
    bool boolean;

    Value(integral_t integral) : integral(integral) { }
    Value(real_t real) : real(real) { }
    Value(bool boolean) : boolean(boolean) { }
  };

  enum class ValueType
  {
    REAL,
    INTEGRAL,
    BOOL
  };

  enum class TokenType
  {
    NONE,
    SKIP,
    
    STRING,
    FLOAT_LITERAL,
    INTEGRAL_LITERAL,
    BOOLEAN_LITERAL,

    OPERATOR
  };

  enum class Operator
  {
    PLUS,
    MINUS,
    DIVIDE,
    MULTIPLY
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
          return isMatching && hasTermination(input, expected.length());
        }

        return false;
      }

      bool hasTermination(std::string_view input, size_t position) const
      {
        return input.length() == position || std::isspace(input[position]);
      }

      std::string_view fetch(std::string_view input) const
      {
        size_t length = 0;
        while (length < input.length() && !std::isspace(input[length])) ++length;
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

        if (p > 0 && (!isNegative || p > 1) && Rule::hasTermination(input, p))
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
      TokenType _type;
      mapping_t _mapping;

    public:
      OperatorRule(TokenType type, const std::initializer_list<std::pair<T, std::string>>& mapping) : _type(type)
      {
        std::transform(mapping.begin(), mapping.end(), std::inserter(_mapping, _mapping.end()), 
                       [](const auto& pair) { return std::make_pair(pair.second, pair.first); 
        });
      }

      Token matches(const std::string_view input) const override
      {
        std::string token = std::string(Rule::fetch(input));

        if (token.length() > 0)
        {
          auto it = _mapping.find(token);

          if (it != _mapping.end())
            return Token(_type, token, it->second);
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

  namespace ast
  {
    using binary_operation = std::function<Value(Value, Value)>;
    
    class Node { };
    class Expression : public Node
    { 
    public:
      virtual ValueType type() const = 0;
    };

    class Value : public Expression
    {
    private:
      ValueType _type;
      nanoexpr::Value _value;

    public:
      template<typename T> Value(ValueType type, T value) : _type(type), _value(value) { }
      ValueType type() const override { return _type; }
      const auto& value() const { return _value; }
    };

    class BinaryExpression : public Expression
    {
    private:
      Operator _op;
      std::unique_ptr<Expression> _left, _right;

    public:
      BinaryExpression(Operator op, Expression* left, Expression* right) : _op(op), _left(left), _right(right) { }
      ValueType type() const override { return ValueType::BOOL; /*TODO*/ }

      const auto& left() { return _left; }
      const auto& right() { return _right; }
      Operator op() { return _op; }
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
          ++token;
        return matched;
      }

      template<typename T, typename std::enable_if<std::is_enum<T>::value && !std::is_same<T, TokenType>::value, T>::type* = nullptr> bool match(TokenType type, const std::initializer_list<T>& tokens)
      {
        if (check(type))
        {
          T value = token->token<T>();
          bool matched = std::find(tokens.begin(), tokens.end(), value) != tokens.end();

          if (matched)
            ++token;

          return matched;
        }

        return false;
      }

      auto previous() const { return std::prev(token); }

      template<typename T, typename std::enable_if<std::is_enum<T>::value && !std::is_same<T, TokenType>::value, T>::type* = nullptr> T previous()
      {
        return std::prev(token)->token<T>();
      }

    protected:
      ast::Node* root() { return addition(); }

      ast::Expression* addition()
      {
        ast::Expression* left = primary();

        while (match(TokenType::OPERATOR, { Operator::PLUS, Operator::MINUS }))
        {
          Operator op = previous<Operator>();
          ast::Expression* right = primary();
          left = new ast::BinaryExpression(op, left, right);
        }

        return left;
      }

      /* primary ::= BOOL | INT | FLOAT | '(' expression ')' */
      ast::Expression* primary()
      {
        if (match(TokenType::BOOLEAN_LITERAL))
          return new ast::Value(ValueType::BOOL, previous()->value());
        else if (match(TokenType::INTEGRAL_LITERAL))
          return new ast::Value(ValueType::INTEGRAL, previous()->value());
        else if (match(TokenType::FLOAT_LITERAL))
          return new ast::Value(ValueType::REAL, previous()->value());

        //TODO: nested expression

        return nullptr;
      }

    public:
      Parser(const lex::token_list& tokens) : tokens(tokens), token(this->tokens.begin()) { }

      ast::Node* parse() { return root(); }
    };
  }


}

nanoexpr::lex::Lexer::Lexer()
{
  rules.emplace_back(new WhiteSpaceRule());
  rules.emplace_back(new BooleanRule());
  rules.emplace_back(new IntegerRule());
  rules.emplace_back(new OperatorRule<Operator>(TokenType::OPERATOR, { { Operator::PLUS, "+" }, { Operator::MINUS, "-" } }));
  
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
    case TokenType::OPERATOR: out << "operator(" << token.textual() << ")"; break;
  }

  return out;
}

int main()
{
  auto input = "6 + 10";

  nanoexpr::lex::Lexer lexer;
  auto result = lexer.parse(input);

  for (const auto& token : result.tokens)
    std::cout << token << std::endl;

  if (!result.success)
    std::cout << "lexer error: " << result.message << std::endl;
  else
  {
    nanoexpr::parser::Parser parser(result.tokens);
    auto ast =  parser.parse();
  }

  
  getchar();

  return 0;
}