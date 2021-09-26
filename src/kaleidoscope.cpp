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

int main(int argc, const char* argv[])
{
    // must give input filename
    // optional -o flag to name to outputted ir file
    // default will be output.ll

    if (argc < 2)
    {
        fprintf(stderr, "Must specify filename.\n");
        return 1;
    }    

    std::string inFileName = argv[1];
    if (inFileName.substr(inFileName.find_last_of(".") + 1) != "ks")
    {
        fprintf(stderr, "Input file must be a .ks file.\n");
        return 1;
    }

    std::string outFileName = "output.ll";

    if (std::strcmp(argv[2], "-o") == 0)
    {
        if (argc < 4)
        {
            fprintf(stderr, "Must specify filename for outputted ir when '-o' flag given.\n");
            return 1;
        }

        outFileName = argv[3];

        for (unsigned i = 4; i < argc; i++)
        {
            fprintf(stderr, "Ignoring argument %s.\n", argv[i]);
        }
    }    

    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();       
    
    kaleidoscope::parser::Parser parser(inFileName, outFileName);
    parser.run();
    parser.finalize();
    
    return 0;
}