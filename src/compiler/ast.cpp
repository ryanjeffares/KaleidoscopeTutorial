#include "ast.h"
#include "logging.h"

using namespace kaleidoscope::ast;

llvm::Value* NumberExprAST::codeGen(kaleidoscope::LLVMTools& llvmTools)
{
    return llvm::ConstantFP::get(*(llvmTools.llvmContext), llvm::APFloat(value));
}

llvm::Value* VariableExprAST::codeGen(kaleidoscope::LLVMTools& llvmTools)
{
    auto v = llvmTools.namedValues[name];
    if (!v)
    {
        logging::logErrorValue("Unknown variable name.");                        
    }
    return v;
} 

llvm::Value* BinaryExprAST::codeGen(kaleidoscope::LLVMTools& llvmTools) 
{
    auto l = lhs->codeGen(llvmTools);
    auto r = rhs->codeGen(llvmTools);

    if (!l || !r)
    {
        return nullptr;
    }

    auto& builder = llvmTools.irBuilder;
    auto& context = llvmTools.llvmContext;
    switch (op)
    {
        case '+':
            return builder->CreateFAdd(l, r, "addtmp");                        
        case '-':
            return builder->CreateFSub(l, r, "subtmp");
        case '/':
            return builder->CreateFDiv(l, r, "divtmp");
        case '*':
            return builder->CreateFMul(l, r, "multmp");
        case '<':
            l = builder->CreateFCmpULT(l, r, "lttmp");
            // convert bool 0/1 to double 0.0/1.0
            return builder->CreateUIToFP(
                l, llvm::Type::getDoubleTy(*context), "boollttmp"
            );
        // case '>':
        //     l = builder->CreateFCmpUGT(l, r, "gttmp");
        //     return builder->CreateUIToFP(
        //         l, llvm::Type::getDoubleTy(*context), "boolgttmp"
        //     );
        default:
            break;                          
    }

    // if it wasn't built in, it must be user defined
    auto func = findFunction(std::string("binary") + op, llvmTools, protos);
    assert(func && "binary operator not found!");
    llvm::Value* ops[2] = { l, r };
    return builder->CreateCall(func, ops, "binop");
}

llvm::Value* UnaryExprAST::codeGen(kaleidoscope::LLVMTools& llvmTools)
{
    auto operandValue = operand->codeGen(llvmTools);
    if (!operandValue)
    {
        return nullptr;
    }    

    auto func = findFunction(std::string("unary") + opcode, llvmTools, protos);
    if (!func)
    {
        logging::logErrorValue("Unknown unary operator.");
        return nullptr;
    }

    return llvmTools.irBuilder->CreateCall(func, operandValue, "unop");
}

llvm::Value* CallExprAST::codeGen(kaleidoscope::LLVMTools& llvmTools)
{    
    auto calleeFunc = findFunction(callee, llvmTools, protos);
    if (!calleeFunc)
    {
        logging::logErrorValue("Unknown function referenced.");
        return nullptr;
    }

    if (calleeFunc->arg_size() != args.size())
    {
        logging::logErrorValue("Incorrect # arguments passed.");
        return nullptr;
    }

    std::vector<llvm::Value*> argsVec;
    for (unsigned i = 0, e = args.size(); i != e; ++i)
    {
        argsVec.push_back(args[i]->codeGen(llvmTools));
        if (!argsVec.back())
        {
            return nullptr;
        }
    }

    return llvmTools.irBuilder->CreateCall(calleeFunc, argsVec, "calltmp");
}

llvm::Function* PrototypeAST::codeGen(kaleidoscope::LLVMTools& llvmTools)
{
    std::vector<llvm::Type*> doubles(args.size(), llvm::Type::getDoubleTy(*(llvmTools.llvmContext)));
    auto fType = llvm::FunctionType::get(llvm::Type::getDoubleTy(*(llvmTools.llvmContext)), doubles, false);
    auto func = llvm::Function::Create(fType, llvm::Function::ExternalLinkage, name, llvmTools.llvmModule.get());

    unsigned index = 0;
    for (auto& arg : func->args())
    {
        arg.setName(args[index++]);
    }

    return func;
}

llvm::Function* FunctionAST::codeGen(kaleidoscope::LLVMTools& llvmTools,
                                     std::map<std::string, std::unique_ptr<PrototypeAST>>& functionProtos)
{
    // transfer ownership of the prototype to the functionProtos map,
    // but keep a reference to it for use below
    auto& p = *proto;
    functionProtos[proto->getName()] = std::move(proto);    
    auto func = findFunction(p.getName(), llvmTools, functionProtos);

    if (!func)
    {
        return nullptr;
    }

    if (p.isBinaryOp())
    {
        binopPrecedence[p.getOperatorName()] = p.getBinaryPrecedence();
    }

    // create a new basic block to start insertion into
    auto block = llvm::BasicBlock::Create(*(llvmTools.llvmContext), "entry", func);
    llvmTools.irBuilder->SetInsertPoint(block);

    // clear the map and record the function arguments there
    llvmTools.namedValues.clear();
    for (auto& arg : func->args())
    {
        llvmTools.namedValues[std::string(arg.getName())] = &arg;
    }

    if (auto returnValue = body->codeGen(llvmTools))
    {
        // finish off the function and validate it
        llvmTools.irBuilder->CreateRet(returnValue);
        llvm::verifyFunction(*func);        
        llvmTools.passManager->run(*func);
        return func;
    }

    // erase the function from memory if there was a problem
    func->eraseFromParent();

    if (p.isBinaryOp())
    {
        binopPrecedence.erase(p.getOperatorName());
    }
    
    return func;
}

llvm::Value* IfExprAST::codeGen(kaleidoscope::LLVMTools& llvmTools)
{
    auto condValue = condition->codeGen(llvmTools);
    if (!condValue)
    {
        return nullptr;
    }

    auto& builder = llvmTools.irBuilder;
    auto& context = llvmTools.llvmContext;

    // convert condition to a bool by comparing non-equal to 0.0
    condValue = builder->CreateFCmpONE(
        condValue, llvm::ConstantFP::get(*context, llvm::APFloat(0.0)), "ifcond"
    );

    auto func = builder->GetInsertBlock()->getParent();

    // create blocks for the then and else cases
    // insert the 'then' block at the end of the function
    auto thenBlock = llvm::BasicBlock::Create(*context, "then", func);
    auto elseBlock = llvm::BasicBlock::Create(*context, "else");
    auto mergeBlock = llvm::BasicBlock::Create(*context, "ifcont");

    builder->CreateCondBr(condValue, thenBlock, elseBlock);

    builder->SetInsertPoint(thenBlock);

    // emit then value
    auto thenValue = thenExpr->codeGen(llvmTools);
    if (!thenValue)
    {
        return nullptr;
    }

    // codegen of 'then' can change the current block, update thenBlock for the PHI
    builder->CreateBr(mergeBlock);
    thenBlock = builder->GetInsertBlock();

    // emit else block
    func->getBasicBlockList().push_back(elseBlock);
    builder->SetInsertPoint(elseBlock);

    auto elseValue = elseExpr->codeGen(llvmTools);
    if (!elseValue)
    {
        return nullptr;
    }

    builder->CreateBr(mergeBlock);
    elseBlock = builder->GetInsertBlock();

    // emit merge block
    func->getBasicBlockList().push_back(mergeBlock);
    builder->SetInsertPoint(mergeBlock);

    auto phiNode = builder->CreatePHI(
        llvm::Type::getDoubleTy(*context), 2, "iftmp"
    );

    phiNode->addIncoming(thenValue, thenBlock);
    phiNode->addIncoming(elseValue, elseBlock);
    return phiNode;
}

llvm::Value* ForExprAST::codeGen(kaleidoscope::LLVMTools& llvmTools)
{
    // emit the start code first, without variable in scope
    auto startValue = start->codeGen(llvmTools);
    if (!startValue)
    {
        return nullptr;
    }

    auto& builder = llvmTools.irBuilder;
    auto& context = llvmTools.llvmContext;

    // make the new basic block for the loop header,
    // inserting after current block
    auto func = builder->GetInsertBlock()->getParent();
    auto preHeaderBlock = builder->GetInsertBlock();
    auto loopBlock = llvm::BasicBlock::Create(*context, "loop", func);
    // insert an explicit fall through from the current block
    // to the loop block
    builder->CreateBr(loopBlock);

    // start insertion into loopBlock
    builder->SetInsertPoint(loopBlock);
    // start the phi node with an entry for start block
    auto variable = builder->CreatePHI(llvm::Type::getDoubleTy(*context), 2, varName.c_str());
    variable->addIncoming(startValue, preHeaderBlock);

    // within the loop, the variable is defined equal to the phi node
    // if it shadows an existing variable, we have to restore it
    // so save it for now
    auto oldVal = llvmTools.namedValues[varName];
    llvmTools.namedValues[varName] = variable;

    // emit the body of the loop.
    // this can change the current block
    // ignore value computed by the body, but dont allow an error
    if (!body->codeGen(llvmTools))
    {
        return nullptr;
    }

    llvm::Value* stepValue = nullptr;
    if (step)
    {        
        stepValue = step->codeGen(llvmTools);
        if (!stepValue)
        {
            return nullptr;
        }
    }
    else
    {        
        // if not specified, use 1.0
        stepValue = llvm::ConstantFP::get(*context, llvm::APFloat(2.0));
    }

    auto nextVar = builder->CreateFAdd(variable, stepValue, "nextvar");

    // compute the end condition
    auto endCond = end->codeGen(llvmTools);
    if (!endCond)
    {
        return nullptr;
    }

    // convert condition to bool
    endCond = builder->CreateFCmpONE(
        endCond, llvm::ConstantFP::get(*context, llvm::APFloat(0.0)), "loopcond"
    );

    // create the after loop block and insert it 
    auto loopEndBlock = builder->GetInsertBlock();
    auto afterBlock = llvm::BasicBlock::Create(*context, "afterloop", func);

    // insert the conditional branch into the end of loopEndBlock
    builder->CreateCondBr(endCond, loopBlock, afterBlock);

    // any new code will be inserted in afterBlock
    builder->SetInsertPoint(afterBlock);

    // add new entry point to the phi node
    variable->addIncoming(nextVar, loopEndBlock);

    // restore the unshadowed variable
    if (oldVal)
    {
        llvmTools.namedValues[varName] = oldVal;
    }
    else 
    {
        llvmTools.namedValues.erase(varName);
    }

    return llvm::Constant::getNullValue(llvm::Type::getDoubleTy(*context));
}