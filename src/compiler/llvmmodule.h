#pragma once

#include <memory>
#include <map>
#include <string>
#include "includellvm.h"
#include "../../include/KaleidoscopeJIT.h"

namespace kaleidoscope
{
    struct LLVMTools
    {
        std::unique_ptr<llvm::LLVMContext> llvmContext;
        std::unique_ptr<llvm::IRBuilder<>> irBuilder;
        std::unique_ptr<llvm::Module> llvmModule;        
        std::map<std::string, llvm::AllocaInst*> namedValues;

        void doInit()
        {
            llvmContext = std::make_unique<llvm::LLVMContext>();
            llvmModule = std::make_unique<llvm::Module>("Kaleidoscope JIT", *llvmContext);
            irBuilder = std::make_unique<llvm::IRBuilder<>>(*llvmContext);
        }
    };
}