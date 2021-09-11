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
        std::unique_ptr<llvm::legacy::FunctionPassManager> passManager;
        std::map<std::string, llvm::Value*> namedValues;          

        void doInit(llvm::orc::KaleidoscopeJIT* kJit)
        {
            llvmContext = std::make_unique<llvm::LLVMContext>();
            llvmModule = std::make_unique<llvm::Module>("Kaleidoscope JIT", *llvmContext);
            irBuilder = std::make_unique<llvm::IRBuilder<>>(*llvmContext);

            llvmModule->setDataLayout(kJit->getDataLayout());

            passManager = std::make_unique<llvm::legacy::FunctionPassManager>(llvmModule.get());
            passManager->add(llvm::createInstructionCombiningPass());
            passManager->add(llvm::createReassociatePass());
            passManager->add(llvm::createGVNPass());
            passManager->add(llvm::createCFGSimplificationPass());            
            passManager->doInitialization();
        }
    };
}