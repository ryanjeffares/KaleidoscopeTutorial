#pragma once

#include <memory>
#include <vector>

#include "includellvm.h"
#include "llvmmodule.h"

namespace kaleidoscope
{
    namespace debug
    {
        // forward declaration
        class ExprAST;

        struct DebugInfo
        {
            std::unique_ptr<llvm::DIBuilder> diBuilder;
            std::vector<llvm::DIScope*> lexicalBlocks;

            llvm::DICompileUnit* compileUnit;
            llvm::DIType* doubleType;            

            void init(llvm::Module&);
            void emitLocation(ExprAST*, kaleidoscope::LLVMTools&);
            
            llvm::DIType* getDoubleType();
        };        
    } // namespace debug    
} // namespace kaleidoscope
