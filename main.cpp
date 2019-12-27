#include <cstdlib>
#include <cstdio>
#include <iostream>

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

struct Foo
{
  int x, y;
};

void test();

int main()
{
  /*test();
  getchar();
  return 0;*/
  
  
  //benchmark<1000000>("(1.0 / (a + 1.0) + 2.0 / (a + 2.0) + 3.0 / (a + 3.0))", [](float a) { return 1.0f / (a + 1.0f) + 2.0f / (a + 2.0f) + 3.0f / (a + 3.0f); }, 2.21f);
  //benchmark<100000>("1.0 + 2.0", [](float a) { return 1.0f + 2.0f; }, 7.89f);
  //benchmark<10000000>("rand() / abs(a*2)", [](float a) { return (rand()%RAND_MAX) / std::abs(a*2); }, 7.89f);
  //benchmark<1000000>("max(abs(a), 15.0f) - 8.0f", [](float a) { return std::max(std::abs(a), 15.0f) - 8.0f; }, 7.89f);

  //getchar();
  //return 0;
  nanoexpr::Compiler compiler;
  auto result2 = compiler.compileToAST("sin(x)^ 2");
  std::cout << result2.ast->textual() << std::endl;
  getchar();

  return 0;
  
  auto input = "wizard.book.researching_spell.school == School::NATURE";
  bool execute = true;

  nanoexpr::lex::Lexer lexer;
  auto lexResult = lexer.parse(input);

  for (const auto& token : lexResult.tokens)
    std::cout << token << std::endl;

  if (!lexResult)
    std::cout << "lexer error: " << lexResult.message << std::endl;
  else if (execute)
  {    
    nanoexpr::parser::Parser parser;
    auto parseResult =  parser.parse(lexResult);

    vm::Engine engine;

    enum class FooEnum { FIELD = 5, OTHER_FIELD = 100};
    engine.enums().registerEnum<FooEnum>("FooEnum", { {"FIELD", FooEnum::FIELD }, {"OTHER_FIELD", FooEnum::OTHER_FIELD } });
    ValueType fooType = engine.mapCustomType<Foo>();

    vm::Environment env = engine.createEnvironment();

    Foo foo = { 5, 10 };
    env.set("foo", TypedValue(&foo, fooType));

    engine.functions().registerNullary("getFoo", fooType, false, [&foo]() { return &foo; });
    engine.functions().registerUnary("getX", ValueType::INTEGRAL, fooType, [](Value v) { return v.as<const Foo*>()->x; });

    //functions->registerUnregisary("getFoo", )

    env.set("a", 2.21f);

    if (parseResult)
    {
      auto compileResult = parseResult->compile(&env);
      std::cout << std::endl << parseResult->textual();

      if (compileResult)
      {
        for (size_t r = 0; r < 1; ++r)
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
  auto parser = nanoexpr::parser::Parser();
  auto parseResult = parser.parse(lexerResult.tokens);

  vm::Engine engine;
  vm::Environment env = engine.createEnvironment();
  env.set("a", arg, false);

  auto result = parseResult.ast->compile(&env);

  for (const auto& token : lexerResult.tokens)
    std::cout << token << std::endl;
  std::cout << std::endl << parseResult.ast->textual() << std::endl;
  
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
  std::cout << "native took " << milli_cast(elapsed[1]).count() << "ms (" << nano_cast(elapsed[1]).count()/COUNT << "ns per iteration)" << std::endl;
  std::cout << "interptreted took " << milli_cast(elapsed[0]).count() << "ms (" << nano_cast(elapsed[0]).count() / COUNT << "ns per iteration)" << std::endl;
  std::cout << "ratio is " << (int)(100* nano_cast(elapsed[0]).count() / nano_cast(elapsed[1]).count()) << "%" << std::endl;
  std::cout << results[0] << " ~= " << results[1] << std::endl;
}