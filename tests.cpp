#define CATCH_CONFIG_MAIN
#include "catch.hpp"

#include "nanoexpr.h"

using namespace nanoexpr;

static const vm::Functions* functions = new vm::Functions();
static const vm::Enums* enums = new vm::Enums();
static const vm::Environment* env = new vm::Environment(functions, enums);

lex::LexerResult lex(const std::string& input) { return lex::Lexer().parse(input); }
parser::ParserResult parse(lex::LexerResult result) { return parser::Parser().parse(result.tokens); }
ast::CompileResult compile(parser::ParserResult result) { return result.ast->compile(env); }

TEST_CASE("configuration test") {
  REQUIRE(true);
}