// https://tc39.es/ecma262/#sec-lexical-grammar
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <exception>
#include <unordered_map>

using std::string;
using std::vector;

class FileNotFoundException : public std::exception {
    virtual const char* what() const throw() {
        return "File not found";
    }
};

class EOFException : public std::exception {};

class Panic : public std::exception {};

class SourceFile {
public:
    SourceFile(const string& filename) {
        file_.open(filename.c_str(), std::ios::in | std::ios::binary);
        if (!file_.is_open()) {
            throw FileNotFoundException();
        }
    }

    bool IsEnd () {
        return file_.eof();
    }

    char Peek () {
        if (IsEnd()) {
            throw EOFException();
        }
        return static_cast<char>(file_.peek());
    }

    char Next () {
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
        OtherPunctuator
    };
    Token (Type type, const string& val) : type(type), value(val) {}
    Type type;
    string value;

    const string ToString() const {
        if (type == Type::DecimalLiteral) {
            return "DecimalLiteral[" + value + "]";
        } else if (type == Type::OtherPunctuator) {
            return "OtherPunctuator[" + value + "]";
        } else {
            throw Panic();
        }
    }
};

Token ReadDigit(SourceFile* file) {
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

vector<Token> lex(SourceFile* file) {
    vector<Token> tokens;
    while (true) {
        const char c = file->Peek();
        if (file->IsEnd()) {
            break;
        }
        if (std::isdigit(static_cast<int>(c))) {
            tokens.push_back(ReadDigit(file));
        } else if (c == '+' || c == '-' || c == ';') {
            tokens.push_back(Token(Token::Type::OtherPunctuator, string(1, file->Next())));
        } else if (c == ' ' || c == '\n') {
            file->Next();
        } else {
            std::cout << static_cast<int>(c) << std::endl;
            throw Panic();
        }
    }
    return tokens;
}

int main () {
    SourceFile file("1.js");
    const auto tokens = lex(&file);
    for (const Token& s : tokens) {
        std::cout << s.ToString() << ", ";
    }
    std::cout << std::endl;
    return EXIT_SUCCESS;
}


