#define CATCH_CONFIG_RUNNER
#include "catch.hpp"

#include "nanoexpr.h"

using namespace nanoexpr;

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

bool checkTokens(const lex::LexerResult& result, const std::vector<TypedValue>& values)
{
  for (size_t i = 0; i < result.tokens.size(); ++i)
    if (!(result.tokens[i] == values[i]))
      return false;
  return true;
}


TEST_CASE("lexer") {
  SECTION("integral literals") {
    REQUIRE(checkTokens(tokenize("0"), { 0 }));
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