#pragma once

#include "common.h"

namespace nanoexpr::lex
{
  enum class TokenType
  {
    NONE, SKIP, ERROR,

    IDENTIFIER,
    STRING, FLOAT, INTEGRAL, BOOLEAN,
    OPERATOR, SYMBOL
  };

  class Token
  {
    TokenType _type;
    std::string _textual;
    Value _value;
    size_t _consumed;

  public:
    Token(TokenType type) : _type(type), _value(0), _consumed(0) { }
    Token(TokenType type, std::string_view textual) : _type(type), _textual(textual), _value(0), _consumed(textual.size()) { }
    Token(TokenType type, std::string_view textual, size_t consumed) : _type(type), _textual(textual), _value(0), _consumed(consumed) { }

    Token(const std::string& message) : _type(TokenType::ERROR), _textual(message), _value(0) { }
    template<typename T, typename std::enable_if<!std::is_enum<T>::value>::type* = nullptr> Token(TokenType type, std::string_view textual, T value) : _type(type), _textual(textual), _value(value), _consumed(textual.size()) { }

    TokenType type() const { return _type; }
    size_t length() const { return _textual.length(); }
    const Value& value() const { return _value; }
    const auto& textual() const { return _textual; }
    const std::string& string() const { return _textual; }
    size_t consumed() const { return _consumed; }

    bool valid() const { return _type != TokenType::NONE && _type != TokenType::ERROR; }

    bool match(TokenType type, const std::string& textual) const { return _type == type && _textual == textual; }
  };

  class Rule
  {
  protected:
    bool lookahead(size_t offset, std::string_view input, char expected, bool ignoreCase = true) const { return offset < input.size() && std::tolower(input[offset]) == expected; }

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
            return Token(TokenType::ERROR, "unclosed string literal");
          else if (input[p] == delim)
            return Token(TokenType::STRING, input.substr(1, p - 1), p + 1);
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
      enum class state { SIGN, BEFORE_DOT, BEFORE_DOT_NO_DIGIT, DIGIT_AFTER_DOT, AFTER_DOT, AFTER_DOT_NO_DIGIT };
      state s = state::SIGN;
      size_t p = 0;

      bool finished = false, failed = false;

      while (p < input.length() && !finished)
      {
        auto c = input[p];

        switch (s)
        {
          case state::SIGN:
            if (c == '-' || c == '+') s = state::BEFORE_DOT_NO_DIGIT;
            else if (std::isdigit(c)) { s = state::BEFORE_DOT; --p; }
            else if (c == '.') { s = state::BEFORE_DOT_NO_DIGIT; --p; }
            else { failed = true; finished = true; }
            break;
          case state::BEFORE_DOT_NO_DIGIT:
          case state::BEFORE_DOT:
            if (std::isdigit(c)) { s = state::BEFORE_DOT; }
            else if (c == '.') { s = s == state::BEFORE_DOT_NO_DIGIT ? state::AFTER_DOT_NO_DIGIT : state::AFTER_DOT; }
            else { failed = true; finished = true; }
            break;
          case state::AFTER_DOT_NO_DIGIT:
            if (std::isdigit(c)) s = state::AFTER_DOT;
            else { failed = true; finished = true; }
            break;
          case state::AFTER_DOT:
            if (Rule::hasSpaceOrNonDigitTermination(input, p))
            {
              finished = true;
              break;
            }
        }

        ++p;
      }

      if (!failed && s == state::AFTER_DOT)
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
    bool isValidDigit(char c, integral_t base) const {
      return (base == 10 && std::isdigit(c)) || (base == 2 && (c == '0' || c == '1')) ||
        (base == 16 && (std::isdigit(c) || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')));
    }

  public:
    Token matches(const std::string_view input) const override
    {
      enum class state { SIGN, BASE, DIGITS, VALID };
      integral_t base = 10;
      bool hasSign = false, finished = false, failed = false;
      size_t p = 0;
      state s = state::SIGN;

      while (p < input.length() && !finished)
      {
        auto c = input[p];

        switch (s) {
          case state::SIGN:
          {
            if (c == '-' || c == '+') { s = state::BASE; hasSign = true; }
            else if (std::isdigit(c)) { s = state::BASE; --p; }
            else { failed = true; finished = true; }
            break;
          }
          case state::BASE:
          {
            if (c == '0' && lookahead(p + 1, input, 'x')) { ++p; s = state::DIGITS; base = 16; }
            else if (c == '0' && lookahead(p + 1, input, 'b')) { ++p; s = state::DIGITS; base = 2; }
            else if (std::isdigit(c)) { s = state::VALID; --p; }
            else { failed = true; finished = true; }
            break;
          }
          case state::DIGITS:
          {
            if (isValidDigit(c, base)) s = state::VALID;
            else { finished = true; failed = true; }
          }

          case state::VALID:
          {
            if (input.length() == p || !isValidDigit(c, base))
            {
              if (std::isalnum(c))
                failed = true;

              --p;
              finished = true;
            }
            break;
          }
        }

        ++p;
      }

      if (!failed && s == state::VALID)
      {
        //TODO: rather hacky, we skip first 2 chars if there's a base specifier then we insert sign if it was present at beginning
        std::string copy = std::string(base != 10 ? input.substr(hasSign ? 3 : 2, p - (hasSign ? 3 : 2) + 1) : input.substr(0, p));
        if (hasSign && base != 10)
          copy.insert(copy.begin(), input[0]);
        return Token(TokenType::INTEGRAL, input.substr(0, p), std::stoi(copy, nullptr, base));
      }
      else
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
    Lexer() noexcept;
    LexerResult parse(const std::string& text);
  };
}

nanoexpr::lex::Lexer::Lexer() noexcept
{
  rules.emplace_back(new WhiteSpaceRule());
  rules.emplace_back(new BooleanRule());
  rules.emplace_back(new FloatingRule());
  rules.emplace_back(new IntegerRule());
  rules.emplace_back(new IdentifierRule());
  rules.emplace_back(new StringRule());
  rules.emplace_back(new OperatorRule(TokenType::OPERATOR, false,
                                      {
                                        "+", "-", "*", "/", "%",
                                        "||", "&&", "!", "~", "|", "&"
                                        "==", "!=", ">=", "<=", ">", "<"
                                      }
  ));
  rules.emplace_back(new OperatorRule(TokenType::SYMBOL, false, { "(", ")", ",", "::", "."/*, ":", "?" */ }));
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
      lex::Token token = rule->matches(input);

      /* a match has been found for this rule */
      if (token.valid())
      {
        if (token.type() == TokenType::ERROR)
          return { {}, false, token.textual() };
        else if (token.type() != TokenType::SKIP)
          tokens.push_back(token);

        p += token.consumed();
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