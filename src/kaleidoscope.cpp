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
    llvm::InitializeAllTargetInfos();
    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetMCs();
    llvm::InitializeAllAsmParsers();
    llvm::InitializeAllAsmPrinters();

    fprintf(stderr, "ready> ");   
    
    kaleidoscope::parser::Parser parser;   
    parser.run();
    if (parser.writeToFile())
    {
        fprintf(stderr, "Wrote to file succesfully.");
    }
}