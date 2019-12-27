#pragma once

#include "common.h"
#include "lexer.h"
#include "vm.h"
#include "parser.h"

#include <variant>

namespace nanoexpr
{
  template<typename T> using callable_script = std::function<T()>;

  using result = std::variant<callable_script<bool>, std::string>;

  class Repository
  {
  public:
    using ident_t = size_t;

  private:
    struct script_holder
    {
      std::function<void(vm::Environment*)> setup;
      std::function<Value()> script;
      script_holder(const decltype(setup)& setup, const decltype(script)& script) : setup(setup), script(script) { }
    };

    std::vector<std::unique_ptr<script_holder>> data;

  private:
    vm::Engine* engine;

    callable_script<bool> compile(const std::string& code, std::function<void(vm::Environment*)> setup)
    {
      using namespace nanoexpr;
      using namespace lex;
      using namespace parser;
      
      Lexer lexer;
      LexerResult lexResult = lexer.parse(code);
      result result;

      if (!lexResult)
        result = "Lexer Result :" + lexResult.message;
      else
      {
        Parser parser;
        ParserResult parseResult = parser.parse(lexResult.tokens);

        if (!parseResult)
          result = "Parser Error: " + parseResult.message;

        return [] { return true; };

        /*
        result = [setup, ]() {
          vm::Engine engine;
          vm::Environment env = engine.createEnvironment();
          setup(&env);
          script = parseResult.ast->compile(env);
        }

        auto script = parseResult.ast->compile();*/
      }
    }
  };

  class Compiler
  {
  public:
    parser::ParserResult compileToAST(const std::string& code)
    {
      lex::Lexer lexer = lex::Lexer();
      lex::LexerResult result = lexer.parse(code);
      
      if (result)
      {
        parser::Parser parser = parser::Parser();
        parser::ParserResult presult = parser.parse(result);

        return presult;
      }
      else
        return { nullptr, "Lexing failed: " + result.message };
    }
  };
}

