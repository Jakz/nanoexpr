#include "nanoexpr.h"

using namespace nanoexpr;

#pragma mark Logging functions

std::ostream& operator<<(std::ostream& out, const lex::Token& token)
{
  switch (token.type()) {
    case lex::TokenType::BOOLEAN: out << "bool(" << (token.value().boolean ? "true" : "false") << ")"; break;
    case lex::TokenType::FLOAT: out << "float(" << token.value().real << ")"; break;
    case lex::TokenType::INTEGRAL: out << "int(" << token.value().integral << ")"; break;
    case lex::TokenType::OPERATOR: out << "operator('" << token.textual() << "')"; break;
    case lex::TokenType::SYMBOL: out << "symbol('" << token.textual() << "')"; break;
    case lex::TokenType::IDENTIFIER: out << "identifier(" << token.textual() << ")"; break;
    case lex::TokenType::STRING: out << "string(" << token.string() << ")"; break;
  }

  return out;
}

std::ostream& operator<<(std::ostream& out, const TypedValue& value)
{
  switch (value.type)
  {
    case ValueType::BOOL: out << "bool(" << (value.value.b() ? "true)" : "false)"); break;
    case ValueType::INTEGRAL: out << "int(" << value.value.i() << ")"; break;
    case ValueType::REAL: out << "real(" << value.value.r() << ")"; break;
  }

  return out;
}

#define CATCH_CONFIG_RUNNER
#include "catch.hpp"

#include <variant>

#pragma mark Utility functions

static const vm::Engine engine;
static const vm::Environment env = engine.createEnvironment();

lex::LexerResult tokenize(const std::string& input) { return lex::Lexer().parse(input); }
parser::ParserResult parse(lex::LexerResult result) { return parser::Parser().parse(result.tokens); }
ast::CompileResult compile(parser::ParserResult result) { return result.ast->compile(&env); }



bool operator==(const lex::Token& token, const TypedValue& v)
{ 
  return 
    (token.type() == lex::TokenType::INTEGRAL && v.type == ValueType::INTEGRAL && token.value().i() == v.value.i()) ||
    (token.type() == lex::TokenType::FLOAT && v.type == ValueType::REAL && token.value().r() == v.value.r()) ||
    (token.type() == lex::TokenType::BOOLEAN && v.type == ValueType::BOOL && token.value().b() == v.value.b());
}

struct TokenCheck
{
  bool isValue;

  union
  {
    TypedValue value;
    lex::Token token;
  };

  TokenCheck(real_t value) : isValue(true), value(value) { }
  TokenCheck(integral_t value) : isValue(true), value(value) { }
  TokenCheck(bool value) : isValue(true), value(value) { }
  TokenCheck(lex::TokenType type, std::string textual) : isValue(false), token(type, textual) { }
  
  TokenCheck(const TokenCheck& other)
  {
    this->isValue = other.isValue;
    if (isValue)
      new (&this->value) TypedValue(other.value);
    else
      new (&this->token) lex::Token(other.token);
  }

  ~TokenCheck()
  {
    if (isValue)
      value.~TypedValue();
    else
      token.~Token();
  }
};

std::ostream& operator<<(std::ostream& out, const TokenCheck& value)
{
  if (value.isValue) return operator<<(out, value.value);
  else return operator<<(out, value.token);
}

bool operator==(const lex::Token& token, const TokenCheck& check) {
  if (check.isValue)
    return token == check.value;
  else
    return token.textual() == check.token.textual() && token.type() == check.token.type();
}

void tokenizeAndCheck(const std::string& input, const std::vector<TokenCheck>& values)
{
  auto result = tokenize(input);
  REQUIRE(result.tokens.size() == values.size());
  for (size_t i = 0; i < result.tokens.size(); ++i)
    REQUIRE(result.tokens[i] == values[i]);
}

void lexerShouldFail(const std::initializer_list<std::string>& inputs)
{
  for (const auto& input : inputs)
    REQUIRE_FALSE(tokenize(input).success);
}

using TT = lex::TokenType;

TEST_CASE("lexer") 
{  
  SECTION("integral literals")
  {
    tokenizeAndCheck("0 1 12 512", { 0, 1, 12, 512 });

    SECTION("alphabetic character are not allowed next to integers")
    {
      REQUIRE_FALSE(tokenize("1m").success);
    }

    SECTION("integral literals can have leading zeroes and signs")
    {
      tokenizeAndCheck("0 00 000 0000", { 0, 0, 0, 0 });
      tokenizeAndCheck("+0 -0 +12 -12 +300 -400", { 0, 0, 12, -12, 300, -400 });
    }

    SECTION("hexadecimal base")
    {
      tokenizeAndCheck("0x00 0x00 0x000 0x0000", { 0, 0, 0, 0 });
      tokenizeAndCheck("+0x00 -0xb0 +0xcc -0Xdd1", { 0, -0xb0, 0xcc, -0xdd1 });


      SECTION("hex base is lexed correctly")
      {
        tokenizeAndCheck("0x01 0x02 0x03 0x04", { 1, 2, 3, 4 });
        tokenizeAndCheck("0x10 0x20 0x30 0x40", { 0x10, 0x20, 0x30, 0x40 });
      }

      SECTION("digits outside decimal range are correctly parsed")
      {
        tokenizeAndCheck("0xAA 0xBB 0xCC 0xDD 0xEE 0xFF", { 0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff });
        tokenizeAndCheck("0X1F 0X2E 0X3D 0X4C 0x5B 0x6A", { 0x1f, 0x2e, 0x3d, 0x4c, 0x5b, 0x6a });
      }

      SECTION("partial numbers or invalid digits are considered erroneous")
      {
        lexerShouldFail({ "0xg1", "0x0k", "0x" });
      }

      SECTION("operators are allowed directly after hex literal")
      {
        tokenizeAndCheck("0x1+", { 1, { TT::OPERATOR, "+" } });
      }
    }

    SECTION("binary base")
    {
      tokenizeAndCheck("0b0 0b00 0b000 0b000", { 0, 0, 0, 0 });

      SECTION("binary base is lexed correctly")
      {
        tokenizeAndCheck("0b1 0b10 0b100 0b1000", { 1, 2, 4, 8 });
        tokenizeAndCheck("0B11 0B101 0B110011 0B001101", { 0b11, 0b101, 0b110011, 0b001101 });
      }

      SECTION("extraneous alphanumeric characters at the end or invalid digits should be erroneous")
      {
        lexerShouldFail({ "0b2", "0b3", "0b4", "0b5", "0b6", "0b7", "0b8", "0b9" });
        lexerShouldFail({ "0b12", "0ba", "0b0c", "0b10101k", "0b" });
      }

      SECTION("operators are allowed directly after binary literal")
      {
        tokenizeAndCheck("0b10101-", { 0b10101, { TT::OPERATOR, "-" } });
      }
    }
  }

  SECTION("floating literals")
  {
    tokenizeAndCheck("0.0 0.00 00.0 00.00 000.000", { 0.0f, 0.0f, 0.0f, 0.0f, 0.0f });

    SECTION("partial floats should be correctly parsed")
    {
      tokenizeAndCheck("0. 1. .0 .01", { 0.0f, 1.0f, 0.0f, 0.01f });
    }
  }

  SECTION("boolean literals")
  {
    tokenizeAndCheck("true false", { true, false });

    SECTION("operators allowed next to boolean constants")
    {
      tokenizeAndCheck("true| false+", { true,{ TT::OPERATOR, "|" }, false,{ TT::OPERATOR, "+" } });
    }

    tokenizeAndCheck("truea falseb", { { TT::IDENTIFIER, "truea" },{ TT::IDENTIFIER, "falseb" } });
  }

  SECTION("string literals")
  {
    tokenizeAndCheck("'foobar'", { { TT::STRING, "foobar" } });
    tokenizeAndCheck("''", { {TT::STRING, ""} });
    lexerShouldFail({ "'foob" });
  }
}


int main(int argc, char* argv[])
{
  int result = Catch::Session().run(argc, argv);

#ifdef _WIN32
  /* prevent console from closing */
  getchar();
#endif

  return result;
}