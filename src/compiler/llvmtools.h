#pragma once

#include <memory>
#include <map>
#include <string>

#include "includellvm.h"
#include "../../include/KaleidoscopeJIT.h"

namespace kaleidoscope
{
    // forward declaration
    namespace ast
    {
        class ExprAST;
    }


    struct DebugInfo
    {
        DebugInfo() = default;        

        void init(llvm::Module&, const std::string& outFileName);
        void emitLocation(ast::ExprAST*, llvm::IRBuilder<>&);

        llvm::DIType* getDoubleType();
        llvm::DISubroutineType* createFunctionType(unsigned numArgs, llvm::DIFile* unit);

        std::vector<llvm::DIScope*> lexicalBlocks;
        std::unique_ptr<llvm::DIBuilder> diBuilder;
        llvm::DICompileUnit* compileUnit;
        llvm::DIType* doubleType;                          
    };        

    struct LLVMTools
    {
        LLVMTools() = default;

        void doInit(llvm::orc::KaleidoscopeJIT* jit, const std::string& outFileName)
        {
            llvmContext = std::make_unique<llvm::LLVMContext>();            
            llvmModule = std::make_unique<llvm::Module>("Kaleidoscope JIT", *llvmContext);
            llvmModule->setDataLayout(jit->getDataLayout());
            irBuilder = std::make_unique<llvm::IRBuilder<>>(*llvmContext);

            llvmModule->addModuleFlag(
                llvm::Module::Warning, "Debug Info Version", llvm::DEBUG_METADATA_VERSION
            );
            if (llvm::Triple(llvm::sys::getProcessTriple()).isOSDarwin())
            {
                llvmModule->addModuleFlag(llvm::Module::Warning, "Dwarf Version", 2);
            }            

            debugInfo.init(*llvmModule, outFileName);
        }

        void finalize()
        {
            debugInfo.diBuilder->finalize();            
            llvmModule->print(llvm::errs(), nullptr);
        }

        void emitLocation(ast::ExprAST* expr)
        {
            debugInfo.emitLocation(expr, *irBuilder);
        }

        DebugInfo debugInfo;

        std::unique_ptr<llvm::LLVMContext> llvmContext;
        std::unique_ptr<llvm::IRBuilder<>> irBuilder;
        std::unique_ptr<llvm::Module> llvmModule;        
        std::map<std::string, llvm::AllocaInst*> namedValues;
    };    
}