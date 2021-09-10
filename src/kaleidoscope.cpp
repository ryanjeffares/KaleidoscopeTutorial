#include "compiler/parser.h"

#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT
#endif

extern "C" DLLEXPORT double putchard(double x)
{
    fputc((char)x, stderr);
    return 0;
}

extern "C" DLLEXPORT double printd(double x)
{
    fprintf(stderr, "%f\n", x);
    return 0;
}

extern "C" DLLEXPORT double cuberoot(double x)
{    
    return std::cbrt(x);
}

int main()
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    fprintf(stderr, "ready> ");   
    
    kaleidoscope::parser::Parser parser;
    parser.initModuleAndPassManager(); 
    parser.run();
    parser.printJitCode();
}