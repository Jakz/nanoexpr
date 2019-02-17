#pragma once

#include "common.h"
#include "lexer.h"
#include "vm.h"

namespace nanoexpr
{
  namespace ast
  {
    struct CompileResult
    {
      bool success;
      ValueType type;
      std::string message;
      vm::lambda_t lambda;

      CompileResult(ValueType type, vm::lambda_t lambda) : success(true), type(type), message(""), lambda(lambda) { }
      CompileResult(std::string&& message) : success(false), type(ValueType::NONE), message(message) { }
      CompileResult() noexcept : success(false) { }

      operator bool() const { return success; }

      static const char* nameForType(ValueType type)
      {
        switch (type) {
          case ValueType::BOOL: return "bool";
          case ValueType::INTEGRAL: return "integral";
          case ValueType::REAL: return "real";
          default: return "n/a";
        }
      }
    };

    class Expression
    {
    protected:

    public:
      virtual bool isFailure() const { return false; }
      virtual CompileResult compile(const vm::Environment* env) const = 0;
      virtual std::string textual(size_t indent = 0) const = 0;
      virtual bool isConstant(const vm::Environment* env) const { return false; }
    };

    class LiteralValue : public Expression
    {
    private:
      ValueType _type;
      Value _value;

    public:
      template<typename T> LiteralValue(ValueType type, T value) : _type(type), _value(value) { }
      bool isConstant(const vm::Environment* env) const override{ return true; }

      CompileResult compile(const vm::Environment*) const override
      {
        return CompileResult(_type, [value = _value]() { return value; });
      }

      std::string textual(size_t indent) const override
      {
        auto result = std::string(indent, ' ') + "value(";
        if (_type == ValueType::INTEGRAL) return result + std::to_string(_value.i()) + ")\n";
        else if (_type == ValueType::REAL) return result + std::to_string(_value.r()) + ")\n";
        else if (_type == ValueType::BOOL) return result + (_value.b() ? "true)\n" : "false)\n");
        else assert(false);
      }
    };

    class Identifier : public Expression
    {
    private:
      std::string _identifier;
      mutable bool _isConstant;

    public:
      Identifier(const std::string& identifier) : _identifier(identifier), _isConstant(false) { }
      bool isConstant(const vm::Environment* env) const override { return _isConstant; } //TODO: should fetch value from env and decide


      CompileResult compile(const vm::Environment* env) const override
      {
        //TODO: here we assume that identifier is already defined in symbol table, is that correct?
        const vm::Environment::variable_t* variable = env->get(_identifier);

        if (variable)
        {
          _isConstant = variable->second;
          return CompileResult(variable->first.type, [value = variable->first.value]() { return value; });
        }
        else
          return CompileResult("identifier not defined '" + _identifier + "'");
      }

      std::string textual(size_t indent = 0) const override { return std::string(indent, ' ') + "identifier(" + _identifier + ", "+(_isConstant?"constant":"non-constant")+")\n"; }
    };

    class EnumValue : public Expression
    {
    private:
      std::string _enumName;
      std::string _enumValue;

    public:
      EnumValue(const std::string& enumName, const std::string& enumValue) : _enumName(enumName), _enumValue(enumValue) { }
      bool isConstant(const vm::Environment* env) const override { return true; }

      CompileResult compile(const vm::Environment* env) const override
      {
        auto value = env->engine().findEnum(_enumName, _enumValue);

        if (value.first)
          return CompileResult(ValueType::INTEGRAL, [value = value.second]() { return value; });

        return CompileResult("Enum value " + _enumName + "::" + _enumValue + " not found");
      }

      std::string textual(size_t indent = 0) const override { return std::string(indent, ' ') + "enum(" + _enumName + "::" + _enumValue + ")\n"; }

    };

    class FunctionCall : public Expression
    {
    private:
      vm::opcode_t _identifier;
      std::vector<std::unique_ptr<Expression>> _args;
      mutable bool _isConstant;
      
    public:
      FunctionCall(const vm::opcode_t& identifier, const std::vector<Expression*>& args) : _identifier(identifier), _isConstant(false)
      {
        for (Expression* arg : args)
          _args.emplace_back(arg);
      }
      
      bool isConstant(const vm::Environment* env) const override { return _isConstant; } // TODO env.functions->find()->isConstant

      CompileResult compile(const vm::Environment* env) const override
      {
        std::vector<CompileResult> args;
        std::transform(_args.begin(), _args.end(), std::back_inserter(args), [env](const auto& expr) { return expr->compile(env); });

        auto failed = std::find_if(args.begin(), args.end(), [](const auto& result) { return !result.success; });

        if (failed != args.end())
          return *failed;
        else
        {
          auto types = std::array<ValueType, 3>({ ValueType::NONE, ValueType::NONE, ValueType::NONE });
          assert(types.size() >= args.size());

          for (size_t i = 0; i < args.size(); ++i)
            types[i] = args[i].type;

          bool retry = true;

          while (retry)
          {
            auto definition = env->engine().findFunction(_identifier, types[0], types[1], types[2]);
            
            if (definition != nullptr)
            {
              const auto& functor = definition->function;
              CompileResult result;
              
              bool anyConstant = false, allConstant = true;
              std::for_each(_args.begin(), _args.end(), [&anyConstant, &allConstant, env] (const auto& arg)
                            { anyConstant |= arg->isConstant(env); allConstant &= arg->isConstant(env); });
              
              const bool optimizeArgs = true && config::OptimizationConstantFolding && (!definition->isConstant || !allConstant);
              const bool optimizeCall = allConstant && config::OptimizationConstantFolding && definition->isConstant;

              switch (_args.size())
              {
                case 0: result = CompileResult(definition->signature.returnType, [function = functor.nullary]() { return function(); }); break;
                case 1:
                {
                  if (optimizeArgs && _args[0]->isConstant(env))
                    result = CompileResult(definition->signature.returnType, [function = functor.unary, first = args[0].lambda()]() { return function(first); });
                  else
                    result = CompileResult(definition->signature.returnType, [function = functor.unary, first = args[0].lambda]() { return function(first()); });
                  break;
                }
                case 2:
                  if (optimizeArgs && _args[0]->isConstant(env))
                  {
                    //std::cout << "Optimizing call to " << _identifier << " (arg0)" << std::endl;
                    result = CompileResult(definition->signature.returnType, [function = functor.binary, first = args[0].lambda(), second = args[1].lambda]() { return function(first, second()); });
                  }
                  else if (optimizeArgs && _args[1]->isConstant(env))
                  {
                    //std::cout << "Optimizing call to " << _identifier << " (arg1)" << std::endl;
                    result = CompileResult(definition->signature.returnType, [function = functor.binary, first = args[0].lambda, second = args[1].lambda()]() { return function(first(), second); });
                  }
                  else
                  {
                    result = CompileResult(definition->signature.returnType, [function = functor.binary, first = args[0].lambda, second = args[1].lambda]() { return function(first(), second()); });
                  }
                  break;

                case 3: result = CompileResult(definition->signature.returnType, [function = functor.ternary, first = args[0].lambda, second = args[1].lambda, third = args[2].lambda]() { return function(first(), second(), third()); }); break;
              }

              if (optimizeCall)
              {
                _isConstant = true;
                //std::cout << "Optimizing call to " << _identifier << std::endl;
                Value value = result.lambda();
                result.lambda = [value = value] { return value; };
              }

              return result;
            }
            else
            {
              /* try with promotion to float if available */
              if (config::EnableIntegerPromotionToReal)
              {
                retry = false;
                for (size_t i = 0; i < types.size(); ++i)
                {
                  if (types[i] == ValueType::INTEGRAL)
                  {
                    args[i].lambda = [old = args[i].lambda]() { Value v = old();  return Value((real_t)v.i()); };
                    types[i] = ValueType::REAL;
                    retry = true;
                  }
                }
              }
            }
          }

          /* no function matching the signature, failure */
          return CompileResult("no matching function found for " + _identifier + " " + CompileResult::nameForType(types[0])); //TODO: finish log

        }

      }

      std::string textual(size_t indent) const override
      {
        std::string result = std::string(indent, ' ') + "call(" + _identifier + ")\n";
        for (const auto& arg : _args)
          result += arg->textual(indent + 2);
        return result;
      }
    };
  }

  namespace parser
  {
    using token_value_t = std::string;
    using namespace lex;

    struct ParserResult
    {
      std::unique_ptr<ast::Expression> ast;
      bool success;
      std::string message;
    };

    class Parser
    {
    protected:
      lex::token_list tokens;
      lex::token_list::const_iterator token;

    protected:
      bool check(TokenType type) { return !finished() && token->type() == type; }

      bool finished() const { return token == tokens.end(); }

      bool match(TokenType type)
      {
        bool matched = check(type);
        if (matched)
          advance();
        return matched;
      }

      bool match(TokenType type, const std::initializer_list<token_value_t> & tokens)
      {
        if (check(type))
        {
          const token_value_t& value = token->textual();
          bool matched = std::find(tokens.begin(), tokens.end(), value) != tokens.end();

          if (matched)
            advance();

          return matched;
        }

        return false;
      }

      auto previous() const
      {
        return std::prev(token);
      }

      bool consume(TokenType type, const std::initializer_list<token_value_t>& tokens, const std::string& error)
      {
        if (!match(type, tokens))
          return false;
        return true;
      }

      void rewind() { --token; }
      void advance() { ++token; }
      const Token& peek() const { return *token; }

    protected:
      template<ast::Expression* (Parser::*function)()>
      ast::Expression* binary(TokenType type, const std::initializer_list<token_value_t>& tokens)
      {
        ast::Expression* left = (this->*function)();

        if (left->isFailure())
          return left;

        while (match(type, tokens))
        {
          vm::opcode_t op = previous()->textual();
          ast::Expression* right = (this->*function)();

          if (right->isFailure())
          {
            delete left;
            return right;
          }

          left = new ast::FunctionCall(op, { left, right });
        }

        return left;
      }

    protected:
      ast::Expression* root()
      {
        ast::Expression* expr = expression();

        if (!finished())
        {
          delete expr;
          return fail("couldn't parse the whole input");
        }

        return expr;
      }

      ast::Expression* expression() { return condition(); }

      /* condition = equality ( [&& ||] condition )* */
      ast::Expression* condition() { return binary<&Parser::equality>(TokenType::OPERATOR, { "&&", "||" }); }

      /* equality = comparison ( [ == != ] comparison ) */
      ast::Expression* equality() { return binary<&Parser::comparison>(TokenType::OPERATOR, { "==", "!=" }); }

      /* comparison = addition ( [ >= <= > < ] addition ) */
      ast::Expression* comparison() { return binary<&Parser::addition>(TokenType::OPERATOR, { ">=", "<=", ">", "<" }); }

      /* addition = multiplication ( [+ -] multiplication )* */
      ast::Expression* addition() { return binary<&Parser::multiplication>(TokenType::OPERATOR, { "+", "-" }); }

      /* multiplication = unary ( [* / %] unary )* */
      ast::Expression* multiplication() { return binary<&Parser::unary>(TokenType::OPERATOR, { "*", "/", "%" }); }

      /* unary = ( [- ~ !] primary )*/
      ast::Expression* unary()
      {
        if (match(TokenType::OPERATOR, { "!", "~", "-" }))
        {
          const token_value_t& op = previous()->textual();
          ast::Expression* operand = call();
          
          if (operand->isFailure())
            return operand;

          return new ast::FunctionCall(op, { operand });
        }

        return call();
      }

      /* call = IDENTIFIER (('.' IDENTIFIER ) | ('(' ( expression ',' )* ')'))+ | unary */
      ast::Expression* call()
      {
        if (match(TokenType::IDENTIFIER))
        {
          ast::Expression* call = nullptr;

          std::string name = previous()->textual();
          std::vector<ast::Expression*> arguments;

          while (match(TokenType::SYMBOL, { "(", "." }))
          {
            /* if it's a dot then we expect an identifier which will become the function name */
            if (previous()->match(TokenType::SYMBOL, "."))
            {
              if (!match(TokenType::IDENTIFIER))
                return fail("expected identifier after member call with '.'");
              else
              {
                /* push old identifier or previous call as first argument and swap function name with this */
                std::string newName = previous()->textual();

                arguments.push_back(call ? call : new ast::Identifier(name));
                name = newName;

                /* if no ( is present after second identifier then we allow a call with no () for empty arguments */
                if (!finished() && peek().match(TokenType::SYMBOL, "("))
                  continue;
                else
                {
                  call = new ast::FunctionCall(name, arguments);
                  arguments.clear();
                }
              }
            }
            else if (previous()->match(TokenType::SYMBOL, "("))
            {
              bool done = false;
              
              while (!done)
              {
                if (finished())
                  return fail("unexpected EOF while parsing function argument list");
                else if (match(TokenType::SYMBOL, { ")" }))
                {
                  call = new ast::FunctionCall(name, arguments);
                  arguments.clear();
                  done = true;
                }
                else if (match(TokenType::SYMBOL, { "," }) && !arguments.empty())
                  ;
                else
                {
                  ast::Expression* argument = expression();

                  if (argument->isFailure())
                  {
                    delete call;
                    return argument;
                  }

                  arguments.push_back(argument);
                }
              }
            }
          }
          

          /* if at least a call has been generated return it, otherwise rewind the identifier and fall back to primary */
          if (call)
            return call;

          rewind();
        }

        return primary();
      }

      /* primary ::= BOOL | INT | FLOAT | '(' expression ')' | IDENTIFIER | IDENTIFIER ('.' IDENTIFIER)? '(' ( expression ',' ) * ')' */
      ast::Expression* primary()
      {
        if (match(TokenType::BOOLEAN))
          return new ast::LiteralValue(ValueType::BOOL, previous()->value());
        else if (match(TokenType::INTEGRAL))
          return new ast::LiteralValue(ValueType::INTEGRAL, previous()->value());
        else if (match(TokenType::FLOAT))
          return new ast::LiteralValue(ValueType::REAL, previous()->value());
        else if (match(TokenType::SYMBOL, { "(" }))
        {
          ast::Expression* expr = expression();
          consume(TokenType::SYMBOL, { ")" }, "expecting ')' after expression");
          return expr;
        }
        else if (match(TokenType::IDENTIFIER))
        {
          if (!finished() && peek().match(TokenType::SYMBOL, "::"))
          {
            std::string enumName = previous()->textual();
            advance();

            if (finished() || !match(TokenType::IDENTIFIER))
              return fail("expecting an identifier after enum name");
            else
            {
              std::string enumValue = previous()->textual();
              return new ast::EnumValue(enumName, enumValue);
            }
          }
          /* it's a raw identifier*/
          else
          {
            return new ast::Identifier(previous()->textual());
          }

        }

        return nullptr;
      }

    public:
      class Failure : public ast::Expression
      {
      private:
        std::string _message;

      public:
        Failure(const std::string& message) : _message(message) { }
        
        virtual ast::CompileResult compile(const vm::Environment* env) const { return ast::CompileResult(); }
        virtual std::string textual(size_t indent = 0) const { return std::string(); }

        bool isFailure() const override { return true; }
        const std::string& message() { return _message; }
      };



      ast::Expression* fail(const std::string& message) { return new Failure(message); }

    public:
      explicit Parser() noexcept { }

      ParserResult parse(const lex::token_list& tokens)
      { 
        this->tokens = tokens;
        this->token = this->tokens.begin();
        ast::Expression* ast = root();
        assert(ast);
        
        if (ast->isFailure())
        {
          ParserResult result = { nullptr, false, static_cast<Failure*>(ast)->message() };
          delete ast;
          return result;
        }
        else
          return { std::unique_ptr<ast::Expression>(ast), true, "" };
      }
    };
  }
}
