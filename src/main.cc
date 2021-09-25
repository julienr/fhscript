#include "fhscript.h"

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
