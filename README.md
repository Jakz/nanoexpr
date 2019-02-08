# NanoExpr

A small lexer/parser/compiler for expressions (and something more) based on C++17 features.

This has been written for hobby and to have something lightweight to embed in other applications, since a full Flex/Bison stack is not always needed (and adds complexity to build phase).

The phases are
* lexing into tokens
* parsing tokens into an abstract syntax tree
* compile the tree recursively by producing a final lambda

So there's no VM which actually executes the code, but C++14 named value bindings to lambda come to rescue and allows nesting lambdas, for example:

`LiteralValue(4.0)` is compiled as `[](vm::Environment* env) { return Value(4.0); }`, while a standard function call like `3.0 + 4.0` is compiled by invoking the *lhs* and *rhs* lambdas and then combine the result, eg:

    std::function<Value()> BinaryFunction::compile()
    {
      auto function = [] (const Value v1, const Value v2) { return v1 + v2; }
      auto left = left->compile();
      auto right = right->compile();
      return [left = left, right = right, function = function] { return function(left(), right()); }
    }
    
Which is rather simple to implement and has the advantage of being easily foldable to another value if we now that what this calculates is constant.

The language supports `int`, `float` and `bool` types for now, many std functions are being added which can be optionally be excluded.
