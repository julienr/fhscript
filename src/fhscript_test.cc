#include "fhscript.h"

#include <gtest/gtest.h>

template <class T>
const T* expectAndCast(const ASTNode* node) {
  EXPECT_EQ(typeid(*node), typeid(T));
  return static_cast<const T*>(node);
}

TEST(ASTTest, Program1) {
  SourceFile file("examples/2.fhs");
  const auto tokens = lex(&file);
  PrintVisitor visitor;

  AST ast(tokens);
  ast.Visit(&visitor);

  {
    auto myFunction = ast.GetFunction("my_function");
    EXPECT_EQ(myFunction->name.value, "my_function");
    // It has 2 arguments
    EXPECT_EQ(myFunction->arguments.size(), 2);
    EXPECT_EQ(myFunction->arguments[0].type, Token::Type::Identifier);
    EXPECT_EQ(myFunction->arguments[0].value, "x");
    EXPECT_EQ(myFunction->arguments[1].type, Token::Type::Identifier);
    EXPECT_EQ(myFunction->arguments[1].value, "y");
    // A while and a return
    auto body = myFunction->body.get();
    EXPECT_EQ(body->statements.size(), 2);
    auto whileNode = expectAndCast<WhileNode>(body->statements[0].get());

    // The y > 0 condition
    auto cond = expectAndCast<BooleanOperatorNode>(whileNode->expression.get());
    auto yVar = expectAndCast<VariableNode>(cond->left.get());
    EXPECT_EQ(yVar->identifier.value, "y");
    auto lit0 = expectAndCast<LiteralNode>(cond->right.get());
    EXPECT_EQ(lit0->literal.value, "0");

    // TODO: Complete
  }
  auto mainFunction = ast.GetFunction("main");
}
