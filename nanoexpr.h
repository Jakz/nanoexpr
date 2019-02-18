#pragma once

#include "common.h"
#include "lexer.h"
#include "vm.h"
#include "parser.h"

namespace nanoexpr
{
  template<typename T> using callable_script = std::function<T()>;

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

    }
  };
}

