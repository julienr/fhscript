// https://tc39.es/ecma262/#sec-lexical-grammar
#include <exception>
#include <fstream>
#include <iostream>
#include <queue>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

using std::queue;
using std::string;
using std::vector;

class FileNotFoundException : public std::exception {
  virtual const char *what() const throw() { return "File not found"; }
};

class EOFException : public std::exception {
  virtual const char *what() const noexcept override { return "EOF"; }
};

class Panic : public std::exception {
public:
  Panic(const std::string &reason) : reason_(reason) {}

protected:
  virtual const char *what() const noexcept override { return reason_.c_str(); }

private:
  std::string reason_;
};

class SourceFile {
public:
  SourceFile(const string &filename) {
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

  char Next() {
    if (IsEnd()) {
      throw EOFException();
    }
    char c;
    file_.get(c);
    return c;
  }

private:
  std::ifstream file_;
};

struct Token {
  enum class Type {
    DecimalLiteral,
    OtherPunctuator,
    KwdFunction,
    KwdVar,
    KwdWhile,
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
  Token(Type type, const string &val) : type(type), value(val) {}
  Type type;
  string value;

  const string ToString() const {
    switch (type) {
    case Type::DecimalLiteral:
      return "DecimalLiteral[" + value + "]";
    case Type::OtherPunctuator:
      return "OtherPunctuator[" + value + "]";
    case Type::KwdFunction:
    case Type::KwdVar:
    case Type::KwdWhile:
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
    {"var", Token::Type::KwdVar},
    {"while", Token::Type::KwdWhile}};

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

Token ReadDigit(SourceFile *file) {
  string digit = "";
  while (true) {
    char c = file->Peek();
    if (file->IsEnd() || !std::isdigit(static_cast<int>(c))) {
      break;
    }
    c = file->Next();
    digit.push_back(c);
  }
  return Token(Token::Type::DecimalLiteral, digit);
}

// Reads either an Identifier or a Keyword
Token ReadLiteral(SourceFile *file) {
  string lit = "";
  while (true) {
    char c = file->Peek();
    if (file->IsEnd() || !(std::isalpha(static_cast<int>(c)) |
                           std::isdigit(static_cast<int>(c)) | c == '_')) {
      break;
    }
    c = file->Next();
    lit.push_back(c);
  }
  if (kKeywords.find(lit) != kKeywords.end()) {
    return Token(kKeywords.at(lit), lit);
  } else {
    return Token(Token::Type::Identifier, lit);
  }
}

vector<Token> lex(SourceFile *file) {
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
      tokens.push_back(Token(kSimpleTokens.at(static_cast<int>(c)),
                             std::string(1, file->Next())));
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

class AST {
public:
  AST(const vector<Token> &tokens);

  struct Node {
    Token token;
    vector<Node> children;
  };

private:
  vector<Node> functions;
};

Token AssertNext(queue<Token> &tokens, Token::Type type) {
  Token t = tokens.front();
  if (t.type != type) {
    throw Panic("Incorrect type");
  }
  tokens.pop();
  return t;
}

AST::Node ConsumeStatement(queue<Token> &tokens) {}

AST::Node ConsumeFunction(queue<Token> &tokens) {
  AssertNext(tokens, Token::Type::KwdFunction);
  auto name = AssertNext(tokens, Token::Type::Identifier);
  // Arguments
  vector<Token> arguments;
  while (tokens.front().type != Token::Type::SepLeftBrace) {
    arguments.push_back(AssertNext(tokens, Token::Type::Identifier));
  }

  AssertNext(tokens, Token::Type::SepLeftBrace);
  // Body
  AssertNext(tokens, Token::Type::SepRightBrace);
}

AST::AST(const vector<Token> &tokens) {
  queue<Token> to_consume;
  for (const Token &t : tokens) {
    to_consume.push(t);
  }
}

int main(int argc, char **argv) {
  if (argc <= 1) {
    std::cerr << "Usage: ./main <program>";
    return EXIT_FAILURE;
  }
  const std::string program_fname = argv[1];
  std::cout << "Reading " << program_fname << std::endl;
  SourceFile file(program_fname);
  const auto tokens = lex(&file);
  for (const Token &s : tokens) {
    std::cout << s.ToString() << " ";
  }
  std::cout << std::endl;
  return EXIT_SUCCESS;
}