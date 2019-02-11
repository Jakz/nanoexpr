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

    public:
      Identifier(const std::string& identifier) : _identifier(identifier) { }
      bool isConstant(const vm::Environment* env) const override { return true; } //TODO: should fetch value from env and decide


      CompileResult compile(const vm::Environment* env) const override
      {
        //TODO: here we assume that identifier is already defined in symbol table, is that correct?
        const TypedValue* value = env->get(_identifier);

        if (value)
          return CompileResult(value->type, [value = value->value]() { return value; });
        else
          return CompileResult("identifier not defined '" + _identifier + "'");
      }

      std::string textual(size_t indent = 0) const override { return std::string(indent, ' ') + "identifier(" + _identifier + ")\n"; }
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
        auto value = env->findEnum(_enumName, _enumValue);

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

    public:
      FunctionCall(const vm::opcode_t& identifier, const std::vector<Expression*>& args) : _identifier(identifier)
      {
        for (Expression* arg : args)
          _args.emplace_back(arg);
      }
      bool isConstant(const vm::Environment* env) const override { return true; } // TODO env.functions->find()->isConstant

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

            auto definition = env->findFunction(_identifier, types[0], types[1], types[2]);

            if (definition != nullptr)
            {
              const auto& functor = definition->function;
              CompileResult result;

              switch (_args.size())
              {
                case 0: result = CompileResult(definition->signature.returnType, [function = functor.nullary]() { return function(); }); break;
                case 1: result = CompileResult(definition->signature.returnType, [function = functor.unary, first = args[0].lambda]() { return function(first()); }); break;
                case 2: result = CompileResult(definition->signature.returnType, [function = functor.binary, first = args[0].lambda, second = args[1].lambda]() { return function(first(), second()); }); break;
                  //TODO: ternary
              }

              if (config::OptimizationConstantFolding && std::all_of(_args.begin(), _args.end(), [env](const auto& arg) { return arg->isConstant(env); }) && definition->isConstant)
              {
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

      void advance() { ++token; }
      const Token& peek() const { return *token; }

    protected:
      template<ast::Expression* (Parser::*function)()>
      ast::Expression* binary(TokenType type, const std::initializer_list<token_value_t>& tokens)
      {
        ast::Expression* left = (this->*function)();

        while (match(type, tokens))
        {
          vm::opcode_t op = previous()->textual();
          ast::Expression* right = (this->*function)();
          left = new ast::FunctionCall(op, { left, right });
        }

        return left;
      }

    protected:
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
          ast::Expression* expr = primary();
          return new ast::FunctionCall(op, { expr });
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
          bool hasParen = peek().match(TokenType::SYMBOL, "("), hasDot = peek().match(TokenType::SYMBOL, ".");
          
          /* it's a function call */
          if (!finished() && (hasParen || hasDot))
          {            
            std::string identifier = previous()->textual();

            advance();
            std::vector<ast::Expression*> arguments;
            bool done = false;

            if (hasDot)
            {
              /* we expect another identifier*/
              if (!match(TokenType::IDENTIFIER))
                return fail("epxected identifier after member call with '.'");
              else
              {
                std::string functionName = previous()->textual();

                if (!peek().match(TokenType::SYMBOL, "("))
                  return fail("expected argument list after member call");

                /* we swap identifiers since first one was object on which function is called */
                advance();
                arguments.push_back(new ast::Identifier(identifier));
                identifier = std::move(functionName);
              }

            }

            /* comma separated expressions as arguments */

            while (!done)
            {
              if (finished())
                done = true; /*TODO: unexpected eof while parsing function arguments */
              else if (match(TokenType::SYMBOL, { ")" }))
                return new ast::FunctionCall(identifier, arguments);
              else if (match(TokenType::SYMBOL, { "," }) && !arguments.empty())
                ;
              else
                arguments.push_back(expression());
            }
          }
          /* it's an enum reference */
          else if (!finished() && peek().match(TokenType::SYMBOL, "::"))
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

      ast::Expression* fail(const std::string& message) { return nullptr; }

    public:
      explicit Parser() noexcept { }

      ParserResult parse(const lex::token_list& tokens)
      { 
        this->tokens = tokens;
        this->token = this->tokens.begin();
        auto* ast = expression();
        return { std::unique_ptr<ast::Expression>(ast), ast != nullptr, "" };
      }
    };
  }
}