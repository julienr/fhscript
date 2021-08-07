// https://tc39.es/ecma262/#sec-lexical-grammar
#include <exception>
#include <fstream>
#include <iostream>
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

class SourceFile {
 public:
  SourceFile(const string& filename) {
    file_.open(filename.c_str(), std::ios::in | std::ios::binary);
    if (!file_.is_open()) {
      throw FileNotFoundException();
    }
  }

  bool IsEnd() { return file_.eof(); }

  char Peek() {
    if (IsEnd()) {
      throw EOFException();
    }
    return static_cast<char>(file_.peek());
  }

  std::tuple<char, Location> Next() {
    const char c = _get();
    return std::make_tuple(c, Location(line_, col_));
  }

 private:
  char _get() {
    if (IsEnd()) {
      throw EOFException();
    }
    char c;
    file_.get(c);
    if (c == '\n') {
      line_++;
      col_ = 0;
    } else {
      col_++;
    }
    return c;
  }
  std::ifstream file_;
  int line_ = 1;
  int col_ = 1;
};

struct Token {
  enum class Type {
    DecimalLiteral,
    OtherPunctuator,
    KwdFunction,
    KwdWhile,
    KwdCall,
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
      case Type::KwdCall:
        return "KwdCall";
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
      case Type::KwdCall:
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
    {"call", Token::Type::KwdCall},
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

Token ReadDigit(SourceFile* file) {
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
Token ReadLiteral(SourceFile* file) {
  string lit = "";
  std::optional<Location> start_loc;
  while (true) {
    char c = file->Peek();
    if (file->IsEnd() || !(std::isalpha(static_cast<int>(c)) |
                           std::isdigit(static_cast<int>(c)) | c == '_')) {
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

vector<Token> lex(SourceFile* file) {
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

  virtual void Visit(Visitor* visitor) const {
    visitor->enter(*this);
    // for (const ASTNode& node : children) {
    // node.Visit(visitor);
    //}
    visitor->exit(*this);
  }

  virtual std::string ToString() const = 0;
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

  // TODO: Store arguments and body
  virtual std::string ToString() const override {
    return std::string("Function(") + name.value + ")";
  }

  virtual void Visit(Visitor* visitor) const override {
    visitor->enter(*this);
    body->Visit(visitor);
    visitor->exit(*this);
  }
};

struct WhileNode : public ASTNode {
  virtual std::string ToString() const override { return "While"; }
};

struct AssignmentNode : public ASTNode {
  virtual std::string ToString() const override { return "Assignment"; }
};

struct ExpressionNode : public ASTNode {
  virtual std::string ToString() const override { return "Expression"; }
};

struct ReturnNode : public ASTNode {
  virtual std::string ToString() const override { return "Return"; }
};

struct FunctionCallNode : public ASTNode {
  virtual std::string ToString() const override { return "FunctionCall"; }
};

class AST {
 public:
  AST(const vector<Token>& tokens);

  void Visit(Visitor* visitor) const {
    for (const auto& node : functions) {
      node->Visit(visitor);
    }
  }

 private:
  vector<unique_ptr<ASTNode>> functions;
};

Token AssertNext(queue<Token>& tokens, Token::Type type) {
  if (tokens.size() == 0) {
    throw Panic("End of file while looking for " + Token::TypeToString(type));
  }
  Token t = tokens.front();
  if (t.type != type) {
    throw Panic("Incorrect type, expected " + Token::TypeToString(type) +
                ", got " + Token::TypeToString(t.type) + ", at " +
                t.location.ToString());
  }
  tokens.pop();
  return t;
}

unique_ptr<ExpressionNode> ConsumeExpression(queue<Token>& tokens,
                                             Token::Type endMarker) {
  vector<Token> statement;
  while (tokens.size() > 0 && tokens.front().type != endMarker) {
    statement.push_back(tokens.front());
    tokens.pop();
  }
  return std::make_unique<ExpressionNode>();
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
    auto expression = ConsumeExpression(tokens, Token::Type::SepLeftBrace);
    auto body = ConsumeBlock(tokens);
    return std::make_unique<WhileNode>();
  } else if (first.type == Token::Type::KwdCall) {
    // Function call
    const Token name = AssertNext(tokens, Token::Type::Identifier);
    // TODO: Consume arguments
    while (tokens.size() > 0 &&
           tokens.front().type != Token::Type::SepSemicolon) {
      tokens.pop();
    }
    AssertNext(tokens, Token::Type::SepSemicolon);
    return std::make_unique<FunctionCallNode>();
  } else if (first.type == Token::Type::Identifier) {
    // Variable assignment
    AssertNext(tokens, Token::Type::OpEqual);
    auto expression = ConsumeExpression(tokens, Token::Type::SepSemicolon);
    AssertNext(tokens, Token::Type::SepSemicolon);
    return std::make_unique<AssignmentNode>();
  } else if (first.type == Token::Type::KwdReturn) {
    auto expression = ConsumeExpression(tokens, Token::Type::SepSemicolon);
    AssertNext(tokens, Token::Type::SepSemicolon);
    return std::make_unique<ReturnNode>();
  } else {
    throw Panic("Invalid statement starting with " +
                Token::TypeToString(first.type));
  }
  /*
  vector<Token> statement;
  while (tokens.size() > 0 &&
         tokens.front().type != Token::Type::SepSemicolon) {
    statement.push_back(tokens.front());
    tokens.pop();
  }
  AssertNext(tokens, Token::Type::SepSemicolon);
  */
  // return ASTNode{// TODO: Placeholder
  //.token =
  // Token(Token::Type::SepSemicolon, ";", Location(42, 42))};
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
    functions.push_back(ConsumeFunction(to_consume));
  }
}

class PrintVisitor : public Visitor {
 public:
  void enter(const ASTNode& node) { std::cout << node.ToString() << std::endl; }
  void exit(const ASTNode& node) {}
};

int main(int argc, char** argv) {
  if (argc <= 1) {
    std::cerr << "Usage: ./main <program>";
    return EXIT_FAILURE;
  }
  const std::string program_fname = argv[1];
  std::cout << "Reading " << program_fname << std::endl;
  SourceFile file(program_fname);
  const auto tokens = lex(&file);
  for (const Token& s : tokens) {
    std::cout << s.ToString() << " ";
  }
  std::cout << std::endl;

  PrintVisitor visitor;

  AST ast(tokens);
  ast.Visit(&visitor);
  return EXIT_SUCCESS;
}
