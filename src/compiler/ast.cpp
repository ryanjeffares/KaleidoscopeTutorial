#include "ast.h"
#include "logging.h"

using namespace kaleidoscope::ast;

llvm::Value* NumberExprAST::codeGen(kaleidoscope::LLVMTools& llvmTools)
{
    return llvm::ConstantFP::get(*(llvmTools.llvmContext), llvm::APFloat(value));
}

llvm::Value* VarExprAST::codeGen(kaleidoscope::LLVMTools& llvmTools)
{
    auto& builder = llvmTools.irBuilder;
    auto& context = llvmTools.llvmContext;

    std::vector<llvm::AllocaInst*> oldBindings;
    
    auto func = builder->GetInsertBlock()->getParent();

    // register all variables and emit their initializer
    for (auto i = 0; i < varNames.size(); i++)
    {
        const std::string& varName = varNames[i].first;
        auto init = varNames[i].second.get();

        // emit the initializer before adding variable to scope
        // this prevents the initializer from referencing the variable itself,
        // and permits stuff like this:
        // var a = 1 in 
        //  var a = a in...
        llvm::Value* initValue;
        if (init)
        {
            initValue = init->codeGen(llvmTools);
            if (!initValue)
            {
                return nullptr;
            }
        }
        else  
        {
            initValue = llvm::ConstantFP::get(*context, llvm::APFloat(0.0));
        }

        auto alloca = createEntryBlockAlloca(func, varName, llvmTools);
        builder->CreateStore(initValue, alloca);

        // remember old binding so we can restore when we unrecurse
        oldBindings.push_back(llvmTools.namedValues[varName]);
        llvmTools.namedValues[varName] = alloca;
    }

    auto bodyValue = body->codeGen(llvmTools);
    if (!bodyValue)
    {
        return nullptr;
    }

    for (auto i = 0; i < varNames.size(); i++)
    {
        llvmTools.namedValues[varNames[i].first] = oldBindings[i];
    }

    return bodyValue;
}

llvm::Value* VariableExprAST::codeGen(kaleidoscope::LLVMTools& llvmTools)
{
    auto alloca = llvmTools.namedValues[name];
    if (!alloca)
    {
        logging::logErrorValue("Unknown variable name.");                        
    }
    return llvmTools.irBuilder->CreateLoad(
        alloca->getAllocatedType(), alloca, name.c_str()
    );
} 

llvm::Value* BinaryExprAST::codeGen(kaleidoscope::LLVMTools& llvmTools) 
{
    auto& builder = llvmTools.irBuilder;
    auto& context = llvmTools.llvmContext;

    // special case for '=' because the lhs should not be emitted as an expression
    if (op == '=')
    {
        // assignment requires the lhs to be an identifier
        auto lhse = dynamic_cast<VariableExprAST*>(lhs.get());
        if (!lhse)
        {
            logging::logErrorValue("Destination of '=' must be a variable.");
            return nullptr;
        }

        // codegen rhs
        auto val = rhs->codeGen(llvmTools);
        if (!val)
        {
            return nullptr;
        }

        // lookup the name
        auto variable = llvmTools.namedValues[lhse->getName()];
        if (!variable)
        {
            logging::logErrorValue("Unknown variable name.");
            return nullptr;
        }

        builder->CreateStore(val, variable);
        return val;
    }

    auto l = lhs->codeGen(llvmTools);
    auto r = rhs->codeGen(llvmTools);

    if (!l || !r)
    {
        return nullptr;
    }
    
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
        case '>':
            l = builder->CreateFCmpUGT(l, r, "gttmp");
            return builder->CreateUIToFP(
                l, llvm::Type::getDoubleTy(*context), "boolgttmp"
            );                    
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
    for (auto i = 0; i < args.size(); i++)
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

static llvm::DISubroutineType* createFunctionType(int numArgs, llvm::DIFile* unity);

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

    auto& builder = llvmTools.irBuilder;

    // create a new basic block to start insertion into
    auto block = llvm::BasicBlock::Create(*(llvmTools.llvmContext), "entry", func);
    builder->SetInsertPoint(block);

    // create a subprogram DIE for this function
    auto& debugInfo = llvmTools.debugInfo;
    llvm::DIFile* unit = debugInfo.diBuilder->createFile(
        debugInfo.compileUnit->getFilename(),
        debugInfo.compileUnit->getDirectory()
    );

    llvm::DIScope* fContext = unit;
    int lineNumber = p.getLine();
    int scopeLine = lineNumber;

    llvm::DISubprogram* subProgram = debugInfo.diBuilder->createFunction(
        fContext,
        p.getName(),
        llvm::StringRef(),
        unit,
        lineNumber,
        createFunctionType(func->arg_size(), unit),
        scopeLine,
        llvm::DINode::FlagPrototyped,
        llvm::DISubprogram::SPFlagDefinition
    );

    // clear the map and record the function arguments there
    llvmTools.namedValues.clear();
    for (auto& arg : func->args())
    {
        auto alloca = createEntryBlockAlloca(func, std::string(arg.getName()), llvmTools);
        builder->CreateStore(&arg, alloca);
        llvmTools.namedValues[std::string(arg.getName())] = alloca;
    }

    if (auto returnValue = body->codeGen(llvmTools))
    {
        // finish off the function and validate it
        builder->CreateRet(returnValue);
        llvm::verifyFunction(*func);                
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
    auto& builder = llvmTools.irBuilder;
    auto& context = llvmTools.llvmContext;

    auto func = builder->GetInsertBlock()->getParent();

    // create an alloca for the variable in the entry block
    auto alloca = createEntryBlockAlloca(func, varName, llvmTools);

    // emit the start code first, without variable in scope
    auto startValue = start->codeGen(llvmTools);
    if (!startValue)
    {
        return nullptr;
    }

    // store the value into the alloca
    builder->CreateStore(startValue, alloca);

    // make the basic block for the loop heaer, inserting an explicit
    // fall through from the current block to the loopBlock.
    // start insertion in loopBlock
    auto loopBlock = llvm::BasicBlock::Create(*context, "loop", func);
    builder->CreateBr(loopBlock);
    builder->SetInsertPoint(loopBlock);

    // within the loop, the variable is defined equal to the PHI node.
    // if it shadows an existing variable, we have to restore it,
    // so save it now.
    auto oldVal = llvmTools.namedValues[varName];
    llvmTools.namedValues[varName] = alloca;

    // emit the body of the loop. This, like any other expr, can change the current block.
    // we ignore the value computed by the body, but don't allow an error
    if (!body->codeGen(llvmTools))
    {
        return nullptr;
    }

    // emit the step value
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
        stepValue = llvm::ConstantFP::get(*context, llvm::APFloat(1.0));
    }

    // compute the end condition
    auto endCondition = end->codeGen(llvmTools);
    if (!endCondition)
    {
        return nullptr;
    }

    // reload, increment, and restore the alloca.
    // This handles the case where the body of the loop mutates the variable

    auto currentVar = builder->CreateLoad(
        alloca->getAllocatedType(), alloca, varName.c_str()
    );
    auto nextVar = builder->CreateFAdd(currentVar, stepValue, "nextvar");
    builder->CreateStore(nextVar, alloca);

    // convert condition to bool
    endCondition = builder->CreateFCmpONE(
        endCondition, llvm::ConstantFP::get(*context, llvm::APFloat(0.0)), "loopcond"
    );

    // create after loop block and insert it 
    auto afterLoopBlock = llvm::BasicBlock::Create(*context, "afterloop", func);

    // insert condition branch into the end of afterLoopBlock
    builder->CreateCondBr(endCondition, loopBlock, afterLoopBlock);

    // any new code will be inserted in afterLoopBlock
    builder->SetInsertPoint(afterLoopBlock);

    // restore the unshadowed variable
    if (oldVal)
    {
        llvmTools.namedValues[varName] = oldVal;
    }
    else 
    {
        llvmTools.namedValues.erase(varName);
    }

    // for expression returns 0
    return llvm::Constant::getNullValue(llvm::Type::getDoubleTy(*context));
}