#include "fhscript.h"

#include <gtest/gtest.h>

template <class T>
const T* expectAndCast(const ASTNode* node) {
  EXPECT_EQ(typeid(*node), typeid(T));
  return static_cast<const T*>(node);
}

TEST(ASTTest, Example2) {
  SourceFile file("examples/2.fhs");
  const auto tokens = lex(&file);

  AST ast(tokens);

  // Useful to debug:
  // PrintVisitor visitor;
  // ast.Visit(&visitor);

  // function my_function
  {
    auto my_function = ast.GetFunction("my_function");
    EXPECT_EQ(my_function->name.value, "my_function");
    // It has 2 arguments
    EXPECT_EQ(my_function->arguments.size(), 2);
    EXPECT_EQ(my_function->arguments[0].type, Token::Type::Identifier);
    EXPECT_EQ(my_function->arguments[0].value, "x");
    EXPECT_EQ(my_function->arguments[1].type, Token::Type::Identifier);
    EXPECT_EQ(my_function->arguments[1].value, "y");
    // A while and a return
    auto body = my_function->body.get();
    EXPECT_EQ(body->statements.size(), 2);
    auto while_node = expectAndCast<WhileNode>(body->statements[0].get());

    // The y > 0 condition
    {
      auto cond =
          expectAndCast<BooleanOperatorNode>(while_node->expression.get());
      auto yVar = expectAndCast<VariableNode>(cond->left.get());
      EXPECT_EQ(yVar->identifier.value, "y");
      auto lit0 = expectAndCast<LiteralNode>(cond->right.get());
      EXPECT_EQ(lit0->literal.value, "0");
    }

    // While body
    {
      auto while_body = while_node->body.get();
      EXPECT_EQ(while_body->statements.size(), 4);
      // y = y - 1
      {
        auto y_assign =
            expectAndCast<AssignmentNode>(while_body->statements[0].get());
        EXPECT_EQ(y_assign->left.type, Token::Type::Identifier);
        EXPECT_EQ(y_assign->left.value, "y");
        auto exp =
            expectAndCast<BooleanOperatorNode>(y_assign->expression.get());
        EXPECT_EQ(exp->op.type, Token::Type::OpMinus);
        EXPECT_EQ(
            expectAndCast<VariableNode>(exp->left.get())->identifier.value,
            "y");
        EXPECT_EQ(expectAndCast<LiteralNode>(exp->right.get())->literal.value,
                  "1");
      }
      // print(42)
      {
        auto func_call =
            expectAndCast<FunctionCallNode>(while_body->statements[2].get());
        EXPECT_EQ(func_call->function_name.value, "print");
        EXPECT_EQ(func_call->arguments.size(), 1);
        EXPECT_EQ(expectAndCast<LiteralNode>(func_call->arguments[0].get())
                      ->literal.value,
                  "42");
      }
      // Nested while
      {
        auto nested_while =
            expectAndCast<WhileNode>(while_body->statements[3].get());
        EXPECT_EQ(nested_while->body->statements.size(), 1);
      }
    }

    // return x
    {
      auto ret = expectAndCast<ReturnNode>(body->statements[1].get());
      EXPECT_EQ(
          expectAndCast<VariableNode>(ret->expression.get())->identifier.value,
          "x");
    }
  }

  // function main
  {
    auto main_function = ast.GetFunction("main");
    auto body = main_function->body.get();
    EXPECT_EQ(body->statements.size(), 3);
    // x = my_function(x, 2)
    auto x_assign = expectAndCast<AssignmentNode>(body->statements[1].get());
    EXPECT_EQ(x_assign->left.value, "x");
    EXPECT_EQ(expectAndCast<FunctionCallNode>(x_assign->expression.get())
                  ->function_name.value,
              "my_function");
  }
}

template <class ConsumeFn>
typename std::result_of<ConsumeFn(std::queue<Token>&)>::type Parse(
    const char* source, ConsumeFn fn) {
  SourceString s(source);
  auto tokens = lex(&s);
  queue<Token> to_consume;
  for (const Token& t : tokens) {
    to_consume.push(t);
  }
  return fn(to_consume);
}

int ParseAndEval(const char* source) {
  auto expr = Parse(source, ConsumeExpression);
  Scope scope;
  return expr->Evaluate(&scope);
}

TEST(Evaluate, Expression) {
  EXPECT_EQ(ParseAndEval("2+3"), 5);
  EXPECT_EQ(ParseAndEval("7-12"), -5);
  EXPECT_EQ(ParseAndEval("4 > 1"), 1);
  EXPECT_EQ(ParseAndEval("2 > 3"), 0);

  EXPECT_EQ(ParseAndEval("2 + (9 - 1)"), 10);
  EXPECT_EQ(ParseAndEval("(3 + 4) - (1 + 2)"), 4);
}

TEST(Evaluate, SimpleAssignment) {
  auto expr = Parse("a = 3;", ConsumeStatement);
  Scope scope;
  expr->Evaluate(&scope);
  EXPECT_EQ(scope.GetVariable("a"), 3);
}

TEST(Evaluate, IncrementVariable) {
  Scope scope;
  Parse("a = 0;", ConsumeStatement)->Evaluate(&scope);
  for (int i = 0; i < 5; ++i) {
    Parse("a = a + 1;", ConsumeStatement)->Evaluate(&scope);
    EXPECT_EQ(scope.GetVariable("a"), i + 1);
  }
}

TEST(Evaluate, FunctionCall) {
  auto my_function = Parse(
      R""""(
        function my_function x {
          return x + 2;
        }
      )"""",
      ConsumeFunction);
  auto main = Parse(
      R""""(
        function main {
          x = my_function(3);
          x = x - 1;
          return x;
        }
      )"""",
      ConsumeFunction);
  Scope scope;
  scope.SetFunction("my_function", my_function.get());
  const Value retval = main->Evaluate(&scope);
  EXPECT_EQ(retval, 4);
}

TEST(Evaluate, While) {
  auto while_node = Parse(
      R""""(
        while y > 0 {
          x = x + 1;
          y = y - 2;
        }
      )"""",
      ConsumeStatement);
  Scope scope;
  scope.SetVariable("x", 0);
  scope.SetVariable("y", 20);
  while_node->Evaluate(&scope);
  EXPECT_EQ(scope.GetVariable("x"), 10);
  EXPECT_EQ(scope.GetVariable("y"), 0);
}

TEST(Evaluate, Program1) {
  SourceString s(
      R""""(
        function add_3 t x {
          i = 0;
          while x > i {
            i = i + 1;
            t = t + 3;
          }
          return t;
        }
        function main {
          return add_3(2, 5);
        }
      )"""");
  const auto tokens = lex(&s);
  AST ast(tokens);
  EXPECT_EQ(ast.Evaluate(), 17);
}
