#include "compiler/parser.h"

int main()
{
    kaleidoscope::parser::Parser parser;
    fprintf(stderr, "ready> ");
    parser.run();
    kaleidoscope::parser::ast::llvmModule->print(llvm::errs(), nullptr);
}