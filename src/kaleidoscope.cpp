#include "compiler/parser.h"

int main()
{
    kaleidoscope::parser::Parser parser;
    fprintf(stderr, "ready> ");    
    parser.run();
    parser.printJitCode();
}