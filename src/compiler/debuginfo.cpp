#include "ast.h"
#include "debuginfo.h"

using namespace kaleidoscope::debug;

void DebugInfo::init(llvm::Module& module)
{
    diBuilder = std::make_unique<llvm::DIBuilder>(module);
    compileUnit = diBuilder->createCompileUnit(
        llvm::dwarf::DW_LANG_C,
        diBuilder->createFile("fib.ks", "."),
        "Kaleidoscope Compiler",
        0,
        "",
        0
    );
}

llvm::DIType* DebugInfo::getDoubleType()
{
    if (doubleType)
    {
        return doubleType;
    }

    doubleType = diBuilder->createBasicType(
        "double", 64, llvm::dwarf::DW_ATE_float
    );
    return doubleType;
}

void DebugInfo::emitLocation(ExprAST* ast, kaleidoscope::LLVMTools& llvmTools)
{
    llvm::DIScope* scope;

    if (lexicalBlocks.empty())
    {
        scope = compileUnit;        
    }
    else
    {
        scope = lexicalBlocks.back();
    }

    llvmTools.irBuilder->SetCurrentDebugLocation(
        llvm::DILocation::get(
            scope->getContext(),
            ast->getLine(),
            ast->getCol(),
            scope
        )
    );
}