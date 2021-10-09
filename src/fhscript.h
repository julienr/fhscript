// https://tc39.es/ecma262/#sec-lexical-grammar
#include <exception>
#include <fstream>
#include <iostream>
#include <memory>
#include <queue>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#define STR(x) #x
#define CHECK(x)                                                              \
  if (!(x)) {                                                                 \
    printf(                                                                   \
        "My custom assertion failed: (%s), function %s, file %s, line %d.\n", \
        STR(x), __PRETTY_FUNCTION__, __FILE__, __LINE__);                     \
    abort();                                                                  \
  }

using std::queue;
using std::string;
using std::unique_ptr;
using std::unordered_set;
using std::vector;

class FileNotFoundException : public std::exception {
  virtual const char* what() const throw() { return "File not found"; }
};

class EOFException : public std::exception {
  virtual const char* what() const noexcept override { return "EOF"; }
};

class Panic : public std::exception {
 public:
  Panic(const std::string& reason) : reason_(reason) {}

 protected:
  virtual const char* what() const noexcept override { return reason_.c_str(); }

 private:
  std::string reason_;
};

struct Location {
  Location(int line, int col) : line(line), col(col) {}

  int line = -1;
  int col = -1;

  std::string ToString() const {
    return "line=" + std::to_string(line) + ", col=" + std::to_string(col);
  }
};

class Source {
 public:
  virtual bool IsEnd() = 0;
  virtual char Peek() = 0;
  std::tuple<char, Location> Next() {
    const char c = GetNextChar();
    return std::make_tuple(c, Location(line_, col_));
  }

 protected:
  virtual char GetNextChar() = 0;

 private:
  char _get() {
    if (IsEnd()) {
      throw EOFException();
    }
    char c = GetNextChar();
    if (c == '\n') {
      line_++;
      col_ = 0;
    } else {
      col_++;
    }
    return c;
  }

  int line_ = 1;
  int col_ = 1;
};

class SourceFile : public Source {
 public:
  SourceFile(const string& filename) {
    file_.open(filename.c_str(), std::ios::in | std::ios::binary);
    if (!file_.is_open()) {
      throw FileNotFoundException();
    }
  }

  virtual bool IsEnd() override { return file_.eof(); }

  virtual char Peek() override {
    if (IsEnd()) {
      throw EOFException();
    }
    return static_cast<char>(file_.peek());
  }

 protected:
  virtual char GetNextChar() override {
    char c;
    file_.get(c);
    return c;
  }

 private:
  std::ifstream file_;
};

// Program source from a string
class SourceString : public Source {
 public:
  SourceString(const string& source) : source_(source) {}

  virtual bool IsEnd() override { return current_ == source_.size(); }

  virtual char Peek() override {
    if (IsEnd()) {
      // Don't throw here to behave like a file where the eof where you can
      // peek once before eofbit is set
      return std::char_traits<char>::eof();
    }
    return source_[current_];
  }

 protected:
  virtual char GetNextChar() override {
    const char c = source_[current_];
    current_++;
    return c;
  }

 private:
  std::string source_;
  int current_ = 0;
};

struct Token {
  enum class Type {
    DecimalLiteral,
    OtherPunctuator,
    KwdFunction,
    KwdWhile,
    KwdReturn,
    Identifier,
    OpPlus,
    OpMinus,
    OpEqual,
    OpGreaterThan,
    SepLeftPar,
    SepRightPar,
    SepLeftBrace,
    SepRightBrace,
    SepSemicolon,
    SepComma,
    SepQuote
  };
  Token(Type type, const string& val, const Location& loc)
      : type(type), value(val), location(loc) {}
  Type type;
  string value;
  Location location;

  bool IsBooleanOperator() const {
    switch (type) {
      case Type::OpPlus:
      case Type::OpMinus:
      case Type::OpGreaterThan:
        return true;
      default:
        return false;
    }
  }

  static const string TypeToString(Token::Type type) {
    switch (type) {
      case Type::DecimalLiteral:
        return "DecimalLiteral";
      case Type::OtherPunctuator:
        return "OtherPunctuator";
      case Type::KwdFunction:
        return "KwdFunction";
      case Type::KwdWhile:
        return "KwdWhile";
      case Type::KwdReturn:
        return "KwdReturn";
      case Type::Identifier:
        return "Identifier";
      case Type::OpPlus:
        return "OpPlus";
      case Type::OpMinus:
        return "OpMinus";
      case Type::OpEqual:
        return "OpEqual";
      case Type::OpGreaterThan:
        return "OpGreaterThan";
      case Type::SepLeftPar:
        return "SepLeftPar";
      case Type::SepRightPar:
        return "SepRightPar";
      case Type::SepLeftBrace:
        return "SepLeftBrace";
      case Type::SepRightBrace:
        return "SepRightBrace";
      case Type::SepSemicolon:
        return "SepSemicolon";
      case Type::SepComma:
        return "SepComma";
      case Type::SepQuote:
        return "SepQuote";
      default:
        throw Panic(std::string("Unhandled type: ") +
                    Token::TypeToString(type));
    }
  }

  const string ToString() const {
    switch (type) {
      case Type::DecimalLiteral:
        return "DecimalLiteral[" + value + "]";
      case Type::OtherPunctuator:
        return "OtherPunctuator[" + value + "]";
      case Type::KwdFunction:
      case Type::KwdWhile:
      case Type::KwdReturn:
        return "Keyword[" + value + "]";
      case Type::Identifier:
        return "Identifier[" + value + "]";
      case Type::OpPlus:
      case Type::OpMinus:
      case Type::OpEqual:
      case Type::OpGreaterThan:
        return value;
      case Type::SepLeftPar:
      case Type::SepRightPar:
      case Type::SepLeftBrace:
      case Type::SepRightBrace:
      case Type::SepSemicolon:
      case Type::SepComma:
      case Type::SepQuote:
        return value;
      default:
        throw Panic(std::string("Unhandled type: ") + value);
    }
  }
};

const std::unordered_map<string, Token::Type> kKeywords = {
    {"function", Token::Type::KwdFunction},
    {"while", Token::Type::KwdWhile},
    {"return", Token::Type::KwdReturn}};

const std::unordered_map<int, Token::Type> kSimpleTokens = {
    {static_cast<int>('+'), Token::Type::OpPlus},
    {static_cast<int>('-'), Token::Type::OpMinus},
    {static_cast<int>('='), Token::Type::OpEqual},
    {static_cast<int>('>'), Token::Type::OpGreaterThan},
    {static_cast<int>('('), Token::Type::SepLeftPar},
    {static_cast<int>(')'), Token::Type::SepRightPar},
    {static_cast<int>('{'), Token::Type::SepLeftBrace},
    {static_cast<int>('}'), Token::Type::SepRightBrace},
    {static_cast<int>(';'), Token::Type::SepSemicolon},
    {static_cast<int>(','), Token::Type::SepComma},
    {static_cast<int>('\''), Token::Type::SepQuote},
};

Token ReadDigit(Source* file) {
  string digit = "";
  std::optional<Location> start_loc;
  while (true) {
    char c = file->Peek();
    if (file->IsEnd() || !std::isdigit(static_cast<int>(c))) {
      break;
    }
    auto [rc, location] = file->Next();
    if (!start_loc) {
      start_loc = location;
    }

    digit.push_back(rc);
  }
  return Token(Token::Type::DecimalLiteral, digit, *start_loc);
}

// Reads either an Identifier or a Keyword
Token ReadLiteral(Source* file) {
  string lit = "";
  std::optional<Location> start_loc;
  while (true) {
    char c = file->Peek();
    if (file->IsEnd() || !(std::isalpha(static_cast<int>(c)) |
                           std::isdigit(static_cast<int>(c)) | (c == '_'))) {
      break;
    }
    auto [rc, location] = file->Next();
    if (!start_loc) {
      start_loc = location;
    }
    lit.push_back(rc);
  }
  if (kKeywords.find(lit) != kKeywords.end()) {
    return Token(kKeywords.at(lit), lit, *start_loc);
  } else {
    return Token(Token::Type::Identifier, lit, *start_loc);
  }
}

vector<Token> lex(Source* file) {
  vector<Token> tokens;
  while (true) {
    const char c = file->Peek();
    if (file->IsEnd()) {
      break;
    }
    if (std::isdigit(static_cast<int>(c))) {
      tokens.push_back(ReadDigit(file));
    } else if (std::isalpha(static_cast<int>(c))) {
      tokens.push_back(ReadLiteral(file));
    } else if (kSimpleTokens.find(static_cast<int>(c)) != kSimpleTokens.end()) {
      auto [rc, location] = file->Next();
      tokens.push_back(Token(kSimpleTokens.at(static_cast<int>(c)),
                             std::string(1, rc), location));
    } else if (c == ' ' || c == '\n') {
      file->Next();
    } else {
      std::cout << "Unhandled input: '" << c
                << "' (code=" << static_cast<int>(c) << ")" << std::endl;
      throw Panic("Unhandled input");
    }
  }
  return tokens;
}

using Value = int;

struct FunctionNode;

struct Scope {
  const FunctionNode* GetFunction(const std::string& name) const {
    const auto it = functions_.find(name);
    if (it == functions_.end()) {
      throw Panic(std::string("Function " + name + " is not defined"));
    } else {
      return it->second;
    }
  }

  void SetFunction(const std::string& name, const FunctionNode* function) {
    functions_[name] = function;
  }

  Value GetVariable(const std::string& name) const {
    const auto it = variables_.find(name);
    if (it == variables_.end()) {
      throw Panic(std::string("Variable " + name + " is not defined"));
    } else {
      return it->second;
    }
  }

  void SetVariable(const std::string& name, const Value& val) {
    variables_[name] = val;
  }

  void Print() const {
    std::cout << "Scope {" << std::endl;
    std::cout << "  variables {" << std::endl;
    for (const auto& kv : variables_) {
      std::cout << "    " << kv.first << "=" << kv.second << std::endl;
    }
    std::cout << "  }" << std::endl;
    std::cout << "  functions {" << std::endl;
    for (const auto& kv : functions_) {
      std::cout << "    " << kv.first << std::endl;
    }
    std::cout << "  }" << std::endl;
    std::cout << "}" << std::endl;
  }

 private:
  std::unordered_map<std::string, Value> variables_;
  std::unordered_map<std::string, const FunctionNode*> functions_;
};

struct ASTNode;

class Visitor {
 public:
  ~Visitor() {}
  virtual void enter(const ASTNode& node) = 0;
  virtual void exit(const ASTNode& node) = 0;
};

struct ASTNode {
  // Token token;
  // vector<ASTNode> children;
  virtual ~ASTNode() {}

  virtual void Visit(Visitor* visitor) const = 0;

  virtual std::string ToString() const = 0;

  virtual Value Evaluate(Scope* scope) const = 0;
};

struct BlockNode : public ASTNode {
  BlockNode(vector<unique_ptr<ASTNode>> statements)
      : statements(std::move(statements)) {}

  vector<unique_ptr<ASTNode>> statements;

  virtual std::string ToString() const override { return "Block"; }

  virtual void Visit(Visitor* visitor) const override {
    visitor->enter(*this);
    for (const auto& node : statements) {
      node->Visit(visitor);
    }
    visitor->exit(*this);
  }

  virtual Value Evaluate(Scope* scope) const override {
    Value retval;
    for (const auto& node : statements) {
      retval = node->Evaluate(scope);
    }
    return retval;
  }
};

struct FunctionNode : public ASTNode {
  FunctionNode(const Token& name, vector<Token> arguments,
               unique_ptr<BlockNode> body)
      : name(name), arguments(std::move(arguments)), body(std::move(body)) {
    CHECK(name.type == Token::Type::Identifier);
  }
  Token name;
  vector<Token> arguments;
  unique_ptr<BlockNode> body;

  virtual std::string ToString() const override {
    return std::string("Function(") + name.value + ")";
  }

  virtual void Visit(Visitor* visitor) const override {
    visitor->enter(*this);
    body->Visit(visitor);
    visitor->exit(*this);
  }

  virtual Value Evaluate(Scope* scope) const {
    Scope func_scope(*scope);
    // We assume this is called from FunctionCallNode which binds the
    // arguments into the scope
    return body->Evaluate(&func_scope);
  }
};

struct ExpressionNode : public ASTNode {
  virtual void Visit(Visitor* visitor) const override {
    visitor->enter(*this);
    visitor->exit(*this);
  }
};

struct FunctionCallNode : public ExpressionNode {
  explicit FunctionCallNode(const Token& name,
                            vector<unique_ptr<ExpressionNode>> arguments)
      : function_name(name), arguments(std::move(arguments)) {}

  Token function_name;
  vector<unique_ptr<ExpressionNode>> arguments;
  virtual std::string ToString() const override {
    return "FunctionCall(" + function_name.value + ", " +
           std::to_string(arguments.size()) + ")";
  }

  virtual void Visit(Visitor* visitor) const override {
    visitor->enter(*this);
    for (const auto& arg : arguments) {
      arg->Visit(visitor);
    }
    visitor->exit(*this);
  }

  virtual Value Evaluate(Scope* scope) const {
    const FunctionNode* function = scope->GetFunction(function_name.value);
    Scope func_scope(*scope);
    // Declaration and bound args should match
    CHECK(arguments.size() == function->arguments.size());
    for (int i = 0; i < arguments.size(); ++i) {
      func_scope.SetVariable(function->arguments[i].value,
                             arguments[i]->Evaluate(scope));
    }
    return function->Evaluate(&func_scope);
  }
};

struct BooleanOperatorNode : public ExpressionNode {
  BooleanOperatorNode(unique_ptr<ExpressionNode> left, const Token& op,
                      unique_ptr<ExpressionNode> right)
      : left(std::move(left)), op(op), right(std::move(right)) {}

  unique_ptr<ExpressionNode> left;
  Token op;
  unique_ptr<ExpressionNode> right;

  virtual std::string ToString() const override {
    return std::string("BooleanOperatorNode[op=") +
           Token::TypeToString(op.type) + std::string("]");
  }

  virtual void Visit(Visitor* visitor) const override {
    visitor->enter(*this);
    left->Visit(visitor);
    right->Visit(visitor);
    visitor->exit(*this);
  }

  virtual Value Evaluate(Scope* scope) const {
    const Value left_value = left->Evaluate(scope);
    const Value right_value = right->Evaluate(scope);

    switch (op.type) {
      case Token::Type::OpPlus:
        return left_value + right_value;
      case Token::Type::OpMinus:
        return left_value - right_value;
      case Token::Type::OpGreaterThan:
        return left_value > right_value ? 1 : 0;
      default:
        throw Panic(std::string("Unhandled boolean operator" +
                                Token::TypeToString(op.type)));
    }
  }
};

struct VariableNode : public ExpressionNode {
  VariableNode(const Token& ident) : identifier(ident) {}
  Token identifier;

  virtual std::string ToString() const override {
    return std::string("VariableNode[") + identifier.ToString() +
           std::string("]");
  }

  virtual Value Evaluate(Scope* scope) const {
    return scope->GetVariable(identifier.value);
  }
};

struct LiteralNode : public ExpressionNode {
  LiteralNode(const Token& literal) : literal(literal) {}
  Token literal;

  virtual std::string ToString() const override {
    return std::string("LiteralNode[") + literal.ToString() + std::string("]");
  }

  virtual Value Evaluate(Scope* scope) const {
    return std::stoi(literal.value);
  }
};

struct WhileNode : public ASTNode {
  WhileNode(unique_ptr<ExpressionNode> expression, unique_ptr<BlockNode> body)
      : expression(std::move(expression)), body(std::move(body)) {}

  unique_ptr<ExpressionNode> expression;
  unique_ptr<BlockNode> body;

  virtual std::string ToString() const override { return "While"; }

  virtual void Visit(Visitor* visitor) const override {
    visitor->enter(*this);
    expression->Visit(visitor);
    body->Visit(visitor);
    visitor->exit(*this);
  }

  virtual Value Evaluate(Scope* scope) const {
    for (;;) {
      const Value condition = expression->Evaluate(scope);
      if (condition == 0) {
        break;
      }
      body->Evaluate(scope);
    }
    return Value();
  }
};

struct AssignmentNode : public ASTNode {
  AssignmentNode(const Token& left, unique_ptr<ExpressionNode> expression)
      : left(left), expression(std::move(expression)) {
    CHECK(left.type == Token::Type::Identifier);
  }
  Token left;
  unique_ptr<ExpressionNode> expression;

  virtual std::string ToString() const override {
    return "Assignment(" + left.value + ")";
  }

  virtual void Visit(Visitor* visitor) const override {
    visitor->enter(*this);
    expression->Visit(visitor);
    visitor->exit(*this);
  }

  virtual Value Evaluate(Scope* scope) const {
    const Value val = expression->Evaluate(scope);
    scope->SetVariable(left.value, val);
    return val;
  }
};

struct ReturnNode : public ASTNode {
  explicit ReturnNode(unique_ptr<ExpressionNode> expression)
      : expression(std::move(expression)) {}

  unique_ptr<ExpressionNode> expression;
  virtual std::string ToString() const override { return "Return"; }

  virtual void Visit(Visitor* visitor) const override {
    visitor->enter(*this);
    expression->Visit(visitor);
    visitor->exit(*this);
  }

  virtual Value Evaluate(Scope* scope) const {
    return expression->Evaluate(scope);
  }
};

class AST {
 public:
  AST(const vector<Token>& tokens);

  void Visit(Visitor* visitor) const {
    for (const auto& node : functions_) {
      node->Visit(visitor);
    }
  }

  const FunctionNode* GetFunction(const std::string& name) const {
    for (const auto& fn : functions_) {
      if (fn->name.value == name) {
        return fn.get();
      }
    }
    throw Panic(std::string("Function not found: ") + name);
  }

  Value Evaluate() const {
    Scope scope;
    for (const auto& func : functions_) {
      scope.SetFunction(func->name.value, func.get());
    }
    const auto main = scope.GetFunction("main");
    return main->Evaluate(&scope);
  }

 private:
  vector<unique_ptr<FunctionNode>> functions_;
};

Token AssertNext(queue<Token>& tokens,
                 const std::unordered_set<Token::Type>& types) {
  std::string expected = "[";
  for (const Token::Type& candidate : types) {
    expected += Token::TypeToString(candidate) + std::string(", ");
  }
  expected += "]";

  if (tokens.size() == 0) {
    throw Panic("End of file while looking for " + expected);
  }
  Token t = tokens.front();
  if (types.find(t.type) == types.end()) {
    throw Panic("Incorrect type, expected one of " + expected + ", got " +
                Token::TypeToString(t.type) + ", at " + t.location.ToString());
  }
  tokens.pop();
  return t;
}

Token AssertNext(queue<Token>& tokens, const Token::Type& type) {
  std::unordered_set types{type};
  return AssertNext(tokens, types);
}

unique_ptr<ExpressionNode> ConsumeExpression(queue<Token>& tokens);

// Parses a function call, but it's the caller responsibility to have
// extracted the function name
unique_ptr<FunctionCallNode> ConsumeFunctionCall(const Token& func_name,
                                                 queue<Token>& tokens) {
  CHECK(func_name.type == Token::Type::Identifier);
  AssertNext(tokens, Token::Type::SepLeftPar);

  vector<unique_ptr<ExpressionNode>> arguments;
  while (tokens.size() > 0 && tokens.front().type != Token::Type::SepRightPar) {
    auto expression = ConsumeExpression(tokens);
    // AssertNext(tokens, {Token::Type::SepComma,
    // Token::Type::SepRightPar});
    arguments.push_back(std::move(expression));
    if (tokens.front().type == Token::Type::SepComma) {
      tokens.pop();
    }
  }
  AssertNext(tokens, Token::Type::SepRightPar);
  return std::make_unique<FunctionCallNode>(func_name, std::move(arguments));
}

// Currently only unary or binary expression (i.e. need explicit parentheses)
// y - 1
// (y - 1)
// y + (x - 1)
// 2 * (x + (y - 3) * 4)
// 2 + my_function(42, (4 + 3))
unique_ptr<ExpressionNode> ConsumeExpression(queue<Token>& tokens) {
  if (tokens.size() == 0) {
    throw Panic("End of file reached while parsing expression");
  }

  unique_ptr<ExpressionNode> left;
  if (tokens.front().type == Token::Type::SepLeftPar) {
    // Parenthesis expression
    //   (2 + 3)
    tokens.pop();
    left = ConsumeExpression(tokens);
    AssertNext(tokens, Token::Type::SepRightPar);
  } else if (tokens.front().type == Token::Type::Identifier) {
    const Token ident = tokens.front();
    tokens.pop();
    // either a variable or function call
    //   x
    //   my_function(42)
    if (tokens.front().type == Token::Type::SepLeftPar) {
      // Function call
      left = ConsumeFunctionCall(ident, tokens);
    } else {
      left = std::make_unique<VariableNode>(ident);
    }
  } else {
    const Token literal = AssertNext(tokens, Token::Type::DecimalLiteral);
    left = std::make_unique<LiteralNode>(literal);
  }

  if (tokens.front().IsBooleanOperator()) {
    const Token op = tokens.front();
    tokens.pop();
    unique_ptr<ExpressionNode> right = ConsumeExpression(tokens);
    auto expr = std::make_unique<BooleanOperatorNode>(std::move(left), op,
                                                      std::move(right));
    return expr;
  } else {
    return left;
  }
}

unique_ptr<BlockNode> ConsumeBlock(queue<Token>& tokens);

unique_ptr<ASTNode> ConsumeStatement(queue<Token>& tokens) {
  if (tokens.size() == 0) {
    throw Panic("End of file reached");
  }
  const Token first = tokens.front();
  tokens.pop();

  if (first.type == Token::Type::KwdWhile) {
    // While loop
    auto expression = ConsumeExpression(tokens);
    auto body = ConsumeBlock(tokens);
    return std::make_unique<WhileNode>(std::move(expression), std::move(body));
  } else if (first.type == Token::Type::Identifier) {
    // Either an assignment or a direct function call, e.g. one of:
    // a = 2 + 1;
    // myFunc(42, 43);
    // a = myFunc(42) + 3;
    if (tokens.front().type == Token::Type::SepLeftPar) {
      auto func_call = ConsumeFunctionCall(first, tokens);
      AssertNext(tokens, Token::Type::SepSemicolon);
      return func_call;
    } else if (tokens.front().type == Token::Type::OpEqual) {
      tokens.pop();
      // Variable assignment
      auto expression = ConsumeExpression(tokens);
      AssertNext(tokens, Token::Type::SepSemicolon);
      return std::make_unique<AssignmentNode>(first, std::move(expression));
    } else {
      throw Panic("Invalid statement, expected ( or =, got " +
                  Token::TypeToString(tokens.front().type));
    }
  } else if (first.type == Token::Type::KwdReturn) {
    auto expression = ConsumeExpression(tokens);
    AssertNext(tokens, Token::Type::SepSemicolon);
    return std::make_unique<ReturnNode>(std::move(expression));
  } else {
    throw Panic("Invalid statement starting with " +
                Token::TypeToString(first.type));
  }
}

unique_ptr<BlockNode> ConsumeBlock(queue<Token>& tokens) {
  AssertNext(tokens, Token::Type::SepLeftBrace);
  vector<unique_ptr<ASTNode>> statements;
  while (tokens.size() > 0 &&
         tokens.front().type != Token::Type::SepRightBrace) {
    statements.push_back(ConsumeStatement(tokens));
  }
  AssertNext(tokens, Token::Type::SepRightBrace);
  return std::make_unique<BlockNode>(std::move(statements));
}

unique_ptr<FunctionNode> ConsumeFunction(queue<Token>& tokens) {
  auto function = AssertNext(tokens, Token::Type::KwdFunction);
  auto name = AssertNext(tokens, Token::Type::Identifier);
  // Arguments
  vector<Token> arguments;
  while (tokens.size() > 0 &&
         tokens.front().type != Token::Type::SepLeftBrace) {
    arguments.push_back(AssertNext(tokens, Token::Type::Identifier));
  }
  auto body = ConsumeBlock(tokens);
  return std::make_unique<FunctionNode>(name, std::move(arguments),
                                        std::move(body));
}

AST::AST(const vector<Token>& tokens) {
  queue<Token> to_consume;
  for (const Token& t : tokens) {
    to_consume.push(t);
  }

  while (to_consume.size() > 0) {
    functions_.push_back(ConsumeFunction(to_consume));
  }
}

class PrintVisitor : public Visitor {
 public:
  void enter(const ASTNode& node) {
    for (int i = 0; i < indent_; ++i) {
      std::cout << "  ";
    }
    std::cout << node.ToString() << std::endl;
    indent_++;
  }
  void exit(const ASTNode& node) { indent_--; }

 private:
  int indent_ = 0;
};
