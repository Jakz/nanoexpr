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
    case ValueType::REAL: out << "real(" << value.value.i() << ")"; break;
    case ValueType::INTEGRAL: out << "int(" << value.value.r() << ")"; break;
  }

  return out;
}

#define CATCH_CONFIG_RUNNER
#include "catch.hpp"

#include <variant>

#pragma mark Utility functions

static const vm::Functions* functions = new vm::Functions();
static const vm::Enums* enums = new vm::Enums();
static const vm::Environment* env = new vm::Environment(functions, enums);

lex::LexerResult tokenize(const std::string& input) { return lex::Lexer().parse(input); }
parser::ParserResult parse(lex::LexerResult result) { return parser::Parser().parse(result.tokens); }
ast::CompileResult compile(parser::ParserResult result) { return result.ast->compile(env); }



bool operator==(const lex::Token& token, const TypedValue& v)
{ 
  return 
    (token.type() == lex::TokenType::INTEGRAL && v.type == ValueType::INTEGRAL && token.value().i() == v.value.i()) ||
    (token.type() == lex::TokenType::FLOAT && v.type == ValueType::REAL && token.value().r() == v.value.r()) ||
    (token.type() == lex::TokenType::BOOLEAN && v.type == ValueType::BOOL && token.value().b() == v.value.b());
}

struct TokenCheck
{
  lex::Token token;

  TokenCheck(real_t value) : token(lex::TokenType::FLOAT, "", value) { }
  TokenCheck(integral_t value) : token(lex::TokenType::INTEGRAL, "", value) { }
  TokenCheck(bool value) : token(lex::TokenType::BOOLEAN, "", value) { }
  TokenCheck(lex::TokenType type, std::string textual) : token(type, textual) { }
};

void checkTokens(const lex::LexerResult& result, const std::vector<TokenCheck>& values)
{
  REQUIRE(result.tokens.size() == values.size());
  for (size_t i = 0; i < result.tokens.size(); ++i)
    REQUIRE(result.tokens[i] == values[i]);
}

TEST_CASE("lexer") 
{


  SECTION("integral literals")
  {
    checkTokens(tokenize("0 1 12 512"), { 0, 1, 12, 512 });
  }

  SECTION("integral literals can have leading zeroes")
  {
    checkTokens(tokenize("0 00 000 0000"), { 0, 0, 0, 0 });
  }

  SECTION("hexadecimal base")
  {
    checkTokens(tokenize("0x00 0x00 0x000 0x0000"), { 0, 0, 0, 0 });

    SECTION(("hex base is converted correctly"))
    {
      checkTokens(tokenize("0x01 0x02 0x03 0x04"), { 1, 2, 3, 4 });
      checkTokens(tokenize("0x10 0x20 0x30 0x40"), { 0x10, 0x20, 0x30, 0x40 });
    }

    SECTION("digits outside decimal range are correctly parsed")
    {
      checkTokens(tokenize("0xAA 0xBB 0xCC 0xDD 0xEE 0xFF"), { 0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff });
      checkTokens(tokenize("0X1F 0X2E 0X3D 0X4C 0x5B 0x6A"), { 0x1f, 0x2e, 0x3d, 0x4c, 0x5b, 0x6a });
    }

    SECTION("partial numbers or invalid digits are considered erroneous")
    {
      REQUIRE_FALSE(tokenize("0xg1").success);
      REQUIRE_FALSE(tokenize("0x0k").success);
      REQUIRE_FALSE(tokenize("0x").success);
    }

    SECTION("operators are allowed directly after literal")
    {
      REQUIRE("0x1+", { 1, "+" });
    }
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