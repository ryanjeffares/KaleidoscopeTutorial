#include "ast.h"
#include "llvmtools.h"

using namespace kaleidoscope;

void DebugInfo::init(llvm::Module& module, const std::string& outFileName)
{
    diBuilder = std::make_unique<llvm::DIBuilder>(module);
    compileUnit = diBuilder->createCompileUnit(
        llvm::dwarf::DW_LANG_C,
        diBuilder->createFile(outFileName, "."),
        "Kaleidoscope Compiler",
        0,
        "",
        0
    );
}

void DebugInfo::emitLocation(ast::ExprAST* ast, llvm::IRBuilder<>& irBuilder)
{
    if (!ast)
    {
        return irBuilder.SetCurrentDebugLocation(llvm::DebugLoc());
    }

    llvm::DIScope* scope = lexicalBlocks.empty() ? compileUnit : lexicalBlocks.back();

    irBuilder.SetCurrentDebugLocation(
        llvm::DILocation::get(
            scope->getContext(),
            ast->getLine(),
            ast->getColumn(),
            scope
        )
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

llvm::DISubroutineType* DebugInfo::createFunctionType(unsigned numArgs, llvm::DIFile* unit)
{
    llvm::SmallVector<llvm::Metadata*, 8> eltTypes;
    llvm::DIType* doubleType = getDoubleType();

    eltTypes.push_back(doubleType);

    for (unsigned i = 0, e = numArgs; i != e; ++i)
    {
        eltTypes.push_back(doubleType);
    }

    return diBuilder->createSubroutineType(
        diBuilder->getOrCreateTypeArray(eltTypes)
    );
}