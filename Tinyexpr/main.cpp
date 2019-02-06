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
#include <cctype>

namespace tinyexpr
{
  using real_t = float;
  using integral_t = int32_t;

  union Value
  {
    real_t real;
    integral_t integral;
    bool boolean;

    Value(integral_t integral) : integral(integral) { }
    Value(real_t real) : real(real) { }
    Value(bool boolean) : boolean(boolean) { }
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
    Token(TokenType type, std::string_view textual, integral_t value) : _type(type), _textual(textual), _value(value) { }

    TokenType type() const { return _type; }
    size_t length() const { return _textual.length(); }
    const Value& value() const { return _value;  }
    const auto& textual() const { return _textual; }

    bool valid() const { return _type != TokenType::NONE; }
  };

  namespace lex
  {
    class Rule
    {
    public:
      virtual Token matches(const std::string_view& input) const = 0;
    };

    class WhiteSpaceRule : public Rule
    {
    public:
      Token matches(const std::string_view& input) const override
      {
        size_t length = 0;
        while (length < input.length() && std::isspace(input[length]))
          ++length;

        if (length > 0)
          return Token(TokenType::SKIP, input.substr(0, length));
        else
          return Token(TokenType::NONE);
      }

      virtual Token generate(std::string_view input) const { return Token(TokenType::SKIP); }
    };

    class KeywordRule : public Rule
    {
    private:
      TokenType _type;
      std::string _keyword;

    public:
      KeywordRule(TokenType type, const std::string& keyword) : _type(type), _keyword(keyword) { }

      Token matches(const std::string_view& input) const override
      {
        if (input.length() >= _keyword.length())
        {
          std::string_view output = input.substr(_keyword.length());

          if (output == _keyword)
            return Token(_type, _keyword);
        }

        return TokenType::NONE;
      }
    };

    class IntegerRule : public Rule
    {
      Token matches(const std::string_view& input) const override
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

        if (p > 0 && (!isNegative || p > 1))
        {
          std::string copy = std::string(input.substr(0, p));
          return Token(TokenType::INTEGRAL_LITERAL, input.substr(0, p), std::stoi(copy)); //TODO: stoi, choose according to integer_t type?
        }

        return TokenType::NONE;
      }
    };

    using token_list = std::vector<Token>;

    class Lexer
    {
    private:
      std::vector<std::unique_ptr<Rule>> rules;

    public:
      Lexer();
      token_list parse(const std::string& text);
    };
  };
 


}

tinyexpr::lex::Lexer::Lexer()
{
  rules.emplace_back(new WhiteSpaceRule());
  rules.emplace_back(new IntegerRule());
}

tinyexpr::lex::token_list tinyexpr::lex::Lexer::parse(const std::string& text)
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
      return {}; //TODO: lex error
  }



  return tokens;
}


using namespace tinyexpr;
std::ostream& operator<<(std::ostream& out, const Token& token)
{
  switch (token.type()) {
    case TokenType::BOOLEAN_LITERAL: out << "bool(" << token.value().boolean << ")"; break;
    case TokenType::FLOAT_LITERAL: out << "float(" << token.value().real << ")"; break;
    case TokenType::INTEGRAL_LITERAL: out << "int(" << token.value().integral << ")"; break;
    case TokenType::OPERATOR: out << "operator(" << token.textual() << ")"; break;
  }

  return out;
}



int main()
{
  auto input = "101 10 15 20    30";

  tinyexpr::lex::Lexer lexer;
  auto result = lexer.parse(input);

  for (const auto& token : result)
    std::cout << token << std::endl;
  
  getchar();

  return 0;
}