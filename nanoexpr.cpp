#include <cstdlib>
#include <cstdio>
#include <iostream>
#include <cassert>

#include "nanoexpr.h"

using namespace nanoexpr;
std::ostream& operator<<(std::ostream& out, const lex::Token& token)
{
  switch (token.type()) {
    case lex::TokenType::BOOLEAN: out << "bool(" << (token.value().boolean ? "true" : "false") << ")"; break;
    case lex::TokenType::FLOAT: out << "float(" << token.value().real << ")"; break;
    case lex::TokenType::INTEGRAL: out << "int(" << token.value().integral << ")"; break;
    case lex::TokenType::OPERATOR: out << "operator('" << token.textual() << "')"; break;
    case lex::TokenType::SYMBOL: out << "symbol('" << token.textual() << "')"; break;
    case lex::TokenType::IDENTIFIER: out << "identifier(" << token.textual() << ")"; break;
    case lex::TokenType::STRING: out << "string(" << token.string()  << ")"; break;
  }

  return out;
}

template<size_t COUNT> void benchmark(const std::string& script, std::function<float(float)> native, float arg);


int main()
{
  benchmark<1000>("(1.0/(a + 1.0) + 2/(a + 2.0)+ 3.0 / (a + 3.0))", [](float a) { return 1.0f / (a + 1.0f) + 2.0f / (a + 2.0f) + 3.0f / (a + 3.0f); }, 2.21f);
  benchmark<1000>("sqrt(a)", [](float a) { return std::sqrt(a); }, 7.89f);

  getchar();
  return 0;
  
  auto input = "(1.0/(a+1)+2/(a+2)+3/(a+3))";
  bool execute = true;

  nanoexpr::lex::Lexer lexer;
  auto lexResult = lexer.parse(input);

  for (const auto& token : lexResult.tokens)
    std::cout << token << std::endl;

  if (!lexResult.success)
    std::cout << "lexer error: " << result.message << std::endl;
  else if (execute)
  {    
    nanoexpr::parser::Parser parser;
    auto parseResult =  parser.parse(lexResult.tokens);

    vm::Functions* functions = new vm::Functions();
    vm::Enums* enums = new vm::Enums();

    enum class FooEnum { FIELD = 5, OTHER_FIELD = 100};
    enums->registerEnum<FooEnum>("FooEnum", { {"FIELD", FooEnum::FIELD }, {"OTHER_FIELD", FooEnum::OTHER_FIELD } });


    vm::Environment env(functions, enums);

    env.set("a", 2.21f);

    if (parseResult.success)
    {
      auto compileResult = parseResult.ast->compile(&env);
      std::cout << std::endl << parseResult.ast->textual();

      if (compileResult)
      {
        Value v = compileResult.lambda();

        FooEnum ev = v.as<FooEnum>();

        std::cout << std::endl << input << " -> ";

        switch (compileResult.type)
        {
          case ValueType::INTEGRAL: std::cout << v.i() << std::endl; break;
          case ValueType::REAL: std::cout << v.r() << std::endl; break;
          case ValueType::BOOL: std::cout << (v.b() ? "true" : "false") << std::endl; break;
        }
      }
      else
        std::cout << "compiler error: " << compileResult.message << std::endl;
    }
    else
      std::cout << "parser error: " << parseResult.message << std::endl;


  }

  getchar();

  return 0;
}

#include <chrono>

template<size_t COUNT>
void benchmark(const std::string& script, std::function<float(float)> native, float arg)
{
  nanoexpr::lex::Lexer lexer;
  auto lexerResult = lexer.parse(script);
  auto parser = nanoexpr::parser::Parser(lexerResult.tokens);
  auto ast = parser.parse();
  
  for (const auto& token : lexerResult.tokens)
    std::cout << token << std::endl;
  std::cout << std::endl << ast->textual() << std::endl;
  
  vm::Functions* functions = new vm::Functions();
  vm::Enums* enums = new vm::Enums();

  vm::Environment env(functions, enums);
  env.set("a", arg);

  auto result = ast->compile(&env);

  float results[2];
  std::chrono::steady_clock::duration elapsed[2];

  if (result)
  {
    {
      auto start = std::chrono::steady_clock::now();
      for (size_t i = 0; i < COUNT; ++i)
      {
        results[0] = result.lambda().as<real_t>();
      }
      auto end = std::chrono::steady_clock::now();

      elapsed[0] = end - start;
    }

    {
      auto start = std::chrono::steady_clock::now();
      for (size_t i = 0; i < COUNT; ++i)
      {
        results[1] = native(arg);
      }
      auto end = std::chrono::steady_clock::now();

      elapsed[1] = end - start;
    }
  }

  using milli_cast = std::chrono::duration<float, std::milli>;
  using nano_cast = std::chrono::duration<float, std::nano>;

  std::cout << "benchmarking " << script << " with " << COUNT << " tries" << std::endl;
  std::cout << "native took " << milli_cast(elapsed[1]).count() << "ms (" << nano_cast(elapsed[1]).count()/COUNT << " per iteration)" << std::endl;
  std::cout << "interptreted took " << milli_cast(elapsed[0]).count() << "ms (" << nano_cast(elapsed[0]).count() / COUNT << " per iteration)" << std::endl;
  std::cout << "ratio is " << (int)(100* nano_cast(elapsed[0]).count() / nano_cast(elapsed[1]).count()) << "%" << std::endl;
  std::cout << results[0] << " ~= " << results[1] << std::endl;
}
