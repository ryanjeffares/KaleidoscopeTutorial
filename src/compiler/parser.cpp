#include "parser.h"

using namespace kaleidoscope::parser;

// =================== AST ===================

llvm::Value* NumberExprAST::codeGen(kaleidoscope::LLVMModule& llvmModule)
{
    return llvm::ConstantFP::get(*(llvmModule.llvmContext), llvm::APFloat(value));
}

llvm::Value* VariableExprAST::codeGen(kaleidoscope::LLVMModule& llvmModule)
{
    auto v = llvmModule.namedValues[name];
    if (!v)
    {
        logging::logErrorValue("Unknown variable name.");                        
    }
    return v;
} 

llvm::Value* BinaryExprAST::codeGen(kaleidoscope::LLVMModule& llvmModule) 
{
    auto l = lhs->codeGen(llvmModule);
    auto r = rhs->codeGen(llvmModule);

    if (!l || !r)
    {
        return nullptr;
    }

    switch (op)
    {
        case '+':
            return llvmModule.irBuilder->CreateFAdd(l, r, "addtmp");                        
        case '-':
            return llvmModule.irBuilder->CreateFSub(l, r, "subtmp");
        case '/':
            return llvmModule.irBuilder->CreateFDiv(l, r, "divtmp");
        case '*':
            return llvmModule.irBuilder->CreateFMul(l, r, "multmp");
        case '<':
            l = llvmModule.irBuilder->CreateFCmpULT(l, r, "lttmp");
            // convert bool 0/1 to double 0.0/1.0
            return llvmModule.irBuilder->CreateUIToFP(
                l, llvm::Type::getDoubleTy(*(llvmModule.llvmContext)), "boollttmp"
            );
        case '>':
            l = llvmModule.irBuilder->CreateFCmpUGT(l, r, "gttmp");
            return llvmModule.irBuilder->CreateUIToFP(
                l, llvm::Type::getDoubleTy(*(llvmModule.llvmContext)), "boolgttmp"
            );
        default:
            logging::logErrorValue("Invalid binary operator.");
            return nullptr;                            
    }
}

llvm::Value* CallExprAST::codeGen(kaleidoscope::LLVMModule& llvmModule)
{
    // auto calleeFunc = llvmModule.getFunction(callee);
    auto calleeFunc = findFunction(callee, llvmModule, protos);
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
        argsVec.push_back(args[i]->codeGen(llvmModule));
        if (!argsVec.back())
        {
            return nullptr;
        }
    }

    return llvmModule.irBuilder->CreateCall(calleeFunc, argsVec, "calltmp");
}

llvm::Function* PrototypeAST::codeGen(kaleidoscope::LLVMModule& llvmModule)
{
    std::vector<llvm::Type*> doubles(args.size(), llvm::Type::getDoubleTy(*(llvmModule.llvmContext)));
    auto fType = llvm::FunctionType::get(llvm::Type::getDoubleTy(*(llvmModule.llvmContext)), doubles, false);
    auto func = llvm::Function::Create(fType, llvm::Function::ExternalLinkage, name, llvmModule.llvmModule.get());

    unsigned index = 0;
    for (auto& arg : func->args())
    {
        arg.setName(args[index++]);
    }

    return func;
}

llvm::Function* FunctionAST::codeGen(kaleidoscope::LLVMModule& llvmModule,
                                     std::map<std::string, std::unique_ptr<PrototypeAST>>& functionProtos)
{
    // transfer ownership of the prototype to the functionProtos map,
    // but keep a reference to it for use below
    auto& p = *proto;
    functionProtos[proto->getName()] = std::move(proto);    
    auto func = findFunction(p.getName(), llvmModule, functionProtos);

    if (!func)
    {
        return nullptr;
    }

    // if (!func->empty())
    // {
    // 	logging::logErrorValue("Function cannot be redefined.");
    // 	return nullptr;
    // }

    // create a new basic block to start insertion into
    auto block = llvm::BasicBlock::Create(*(llvmModule.llvmContext), "entry", func);
    llvmModule.irBuilder->SetInsertPoint(block);

    // clear the map and record the function arguments there
    llvmModule.namedValues.clear();
    for (auto& arg : func->args())
    {
        llvmModule.namedValues[std::string(arg.getName())] = &arg;
    }

    if (auto returnValue = body->codeGen(llvmModule))
    {
        // finish off the function and validate it
        llvmModule.irBuilder->CreateRet(returnValue);
        llvm::verifyFunction(*func);        
        llvmModule.passManager->run(*func);
        return func;
    }

    // erase the function from memory if there was a problem
    func->eraseFromParent();
    return func;
}

llvm::Value* IfExprAST::codeGen(kaleidoscope::LLVMModule& llvmModule)
{
    auto condValue = condition->codeGen(llvmModule);
    if (!condValue)
    {
        return nullptr;
    }

    auto& builder = llvmModule.irBuilder;

    // convert condition to a bool by comparing non-equal to 0.0
    condValue = builder->CreateFCmpONE(
        condValue, llvm::ConstantFP::get(*(llvmModule.llvmContext), llvm::APFloat(0.0)), "ifcond");

    auto func = builder->GetInsertBlock()->getParent();

    // create blocks for the then and else cases
    // insert the 'then' block at the end of the function
    auto thenBlock = llvm::BasicBlock::Create(*(llvmModule.llvmContext), "then", func);
    auto elseBlock = llvm::BasicBlock::Create(*(llvmModule.llvmContext), "else");
    auto mergeBlock = llvm::BasicBlock::Create(*(llvmModule.llvmContext), "ifcont");

    builder->CreateCondBr(condValue, thenBlock, elseBlock);

    builder->SetInsertPoint(thenBlock);

    // emit then value
    auto thenValue = thenExpr->codeGen(llvmModule);
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

    auto elseValue = elseExpr->codeGen(llvmModule);
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
        llvm::Type::getDoubleTy(*(llvmModule.llvmContext)), 2, "iftmp");

    phiNode->addIncoming(thenValue, thenBlock);
    phiNode->addIncoming(elseValue, elseBlock);
    return phiNode;
}

llvm::Value* ForExprAST::codeGen(kaleidoscope::LLVMModule& llvmModule)
{
    // emit the start code first, without variable in scope
    auto startValue = start->codeGen(llvmModule);
    if (!startValue)
    {
        return nullptr;
    }

    auto& builder = llvmModule.irBuilder;
    auto& context = llvmModule.llvmContext;

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
    auto oldVal = llvmModule.namedValues[varName];
    llvmModule.namedValues[varName] = variable;

    // emit the body of the loop.
    // this can change the current block
    // ignore value computed by the body, but dont allow an error
    if (!body->codeGen(llvmModule))
    {
        return nullptr;
    }

    llvm::Value* stepValue = nullptr;
    if (step)
    {        
        stepValue = step->codeGen(llvmModule);
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
    auto endCond = end->codeGen(llvmModule);
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
        llvmModule.namedValues[varName] = oldVal;
    }
    else 
    {
        llvmModule.namedValues.erase(varName);
    }

    return llvm::Constant::getNullValue(llvm::Type::getDoubleTy(*context));
}

// =================== Parser ===================

Parser::Parser()
{
    kaleidoscopeJit = exitOnError(llvm::orc::KaleidoscopeJIT::Create());    
    binopPrecedence['<'] = 10;
    binopPrecedence['>'] = 20;
    binopPrecedence['+'] = 30;
    binopPrecedence['-'] = 40;
    binopPrecedence['/'] = 50;
    binopPrecedence['*'] = 60;
}	

void Parser::run()
{
    initModuleAndPassManager();
    getNextToken();    
    mainLoop();
}

void Parser::mainLoop()
{                
    while (1)
    {
        fprintf(stderr, "ready> ");
        switch (currentToken)
        {
            case Lexer::Token::TOK_EOF:
                return;
            case Lexer::Token::TOK_DEF:
                handleDefinition();
                break;
            case Lexer::Token::TOK_EXTERN:
                handleExtern();
                break;
            case ';':
                getNextToken();
                break;
            default:
                handleTopLevelExpression();
                break;
        }
    }
}

void Parser::initModuleAndPassManager()
{
    llvmModule.doInit(kaleidoscopeJit.get());
}

void Parser::handleDefinition()
{
    if (auto funcAst = parseDefinition())
    {
        if (auto funcIr = funcAst->codeGen(llvmModule, functionProtos))
        {
            fprintf(stderr, "Read function definition: \n");
            funcIr->print(llvm::errs());
            fprintf(stderr, "\n");
            exitOnError(kaleidoscopeJit->addModule(
                llvm::orc::ThreadSafeModule(std::move(llvmModule.llvmModule), 
                std::move(llvmModule.llvmContext))));
            initModuleAndPassManager();
        }					
    }
    else 
    {
        getNextToken();
    }
}

void Parser::handleExtern()
{
    if (auto funcAst = parseExtern())
    {
        if (auto funcIr = funcAst->codeGen(llvmModule))
        {
            fprintf(stderr, "Read extern: \n");
            funcIr->print(llvm::errs());
            fprintf(stderr, "\n");
            functionProtos[funcAst->getName()] = std::move(funcAst);
        }					
    }
    else
    {
        getNextToken();
    }
}

void Parser::handleTopLevelExpression()
{
    if (auto funcAst = parseTopLevelExpr())
    {
        if (funcAst->codeGen(llvmModule, functionProtos))
        {
            // jit the module containing the anonymous expression,
            // keeping a handle on it so we can free it later
            auto resourceTracker = kaleidoscopeJit->getMainJITDylib().createResourceTracker();
            
            auto tsm = llvm::orc::ThreadSafeModule(std::move(llvmModule.llvmModule), 
                std::move(llvmModule.llvmContext));
            exitOnError(kaleidoscopeJit->addModule(std::move(tsm), resourceTracker));
            initModuleAndPassManager();

            // search the jit for the __anon_expr symbol
            auto exprSymbol = exitOnError(kaleidoscopeJit->lookup("__anon_expr"));

            // get the symbol's address and cast it to the right type
            // so we can call it as a native function
            double (*fp)() = (double (*)())(intptr_t)exprSymbol.getAddress();            
            fprintf(stderr, "%f\n", fp());
                     
            exitOnError(resourceTracker->remove());
        }
    }
    else 
    {
        getNextToken();
    }
}	

std::unique_ptr<ExprAST> Parser::parsePrimary()
{
    switch (currentToken) 
    {
        case Lexer::Token::TOK_IDENT:
            return parseIdentifierExpr();
        case Lexer::Token::TOK_NUM:
            return parseNumberExpr();
        case Lexer::Token::TOK_IF:
            return parseIfExpr();
        case Lexer::Token::TOK_FOR:
            return parseForExpr();
        case '(':
            return parseParenExpr();
        default:
            logging::logErrorToken("Unknown token when expecting an expression.");
            return nullptr;
    }
}

std::unique_ptr<PrototypeAST> Parser::parsePrototype()
{
    if (currentToken != Lexer::Token::TOK_IDENT)
    {
        logging::logErrorProto("Expected function name in prototype.");
        return nullptr;
    }

    std::string funcName = lexer.getIdentStr();
    getNextToken();

    if (currentToken != '(')
    {
        logging::logErrorProto("Expected '(' in prototype.");
        return nullptr;
    }

    std::vector<std::string> argNames;
    while (getNextToken() == Lexer::Token::TOK_IDENT)
    {
        argNames.push_back(lexer.getIdentStr());
    }

    if (currentToken != ')')
    {
        logging::logErrorProto("Expected a ')' in prototype.");
        return nullptr;
    }

    getNextToken();
    return std::make_unique<PrototypeAST>(funcName, std::move(argNames));
}
            
std::unique_ptr<FunctionAST> Parser::parseDefinition()
{
    getNextToken();

    auto proto = parsePrototype();

    if (!proto)
    {
        return nullptr;
    }

    if (auto e = parseExpression())
    {
        return std::make_unique<FunctionAST>(std::move(proto), std::move(e));
    }

    return nullptr;
}
            
std::unique_ptr<PrototypeAST> Parser::parseExtern()
{
    getNextToken();
    return parsePrototype();    
}

std::unique_ptr<FunctionAST> Parser::parseTopLevelExpr()
{
    if (auto e = parseExpression())
    {
        auto proto = std::make_unique<PrototypeAST>("__anon_expr", std::vector<std::string>());
        return std::make_unique<FunctionAST>(std::move(proto), std::move(e));
    }
    return nullptr;
}

// gets the left hand side of a binary expression, then gets the right
std::unique_ptr<ExprAST> Parser::parseExpression()
{
    auto lhs = parsePrimary();
    if (!lhs)
    {
        return nullptr;
    }
    return parseBinOpRhs(0, std::move(lhs));
}

// gets the right hand side of a binary expression
std::unique_ptr<ExprAST> Parser::parseBinOpRhs(int exprPrec, std::unique_ptr<ExprAST> lhs)
{
    while (1)
    {
        int tokPrec = getTokPrecedence();

        // if this is a binop that binds at least as tightly as the current,
        // consume it, otherwise we are done
        if (tokPrec < exprPrec)
        {
            return lhs;
        }

        // we know this is a binop
        int binOp = currentToken;
        // eat binop
        getNextToken();

        auto rhs = parsePrimary();
        if (!rhs)
        {
            return nullptr;
        }

        // if binop binds less tightly with rhs than the operator afte rhs,
        // let the pending operator take rhs as its lhs
        int nextPrec = getTokPrecedence();
        if (tokPrec < nextPrec)
        {
            rhs = parseBinOpRhs(tokPrec + 1, std::move(rhs));
            if (!rhs)
            {
                return nullptr;
            }
        }

        // merge lhs/rhs
        lhs = std::make_unique<BinaryExprAST>(binOp, std::move(lhs), std::move(rhs));
    }
}
            
// handles variable references and function calls
std::unique_ptr<ExprAST> Parser::parseIdentifierExpr()
{
    std::string idName = lexer.getIdentStr();
    // eat the identifier
    getNextToken();

    // simple variable reference
    if (currentToken != '(')
    {
        return std::make_unique<VariableExprAST>(idName);
    }

    // eat the token
    getNextToken();
    std::vector<std::unique_ptr<ExprAST>> args;
    if (currentToken != ')')
    {
        while (1)
        {
            if (auto arg = parseExpression())
            {
                args.push_back(std::move(arg));
            }
            else 
            {
                return nullptr;
            }

            if (currentToken == ')')
            {
                break;
            }

            if (currentToken != ',')
            {
                logging::logErrorToken("Expected a ')' or ',' in argument list.");
                return nullptr;
            }

            getNextToken();
        }
    }

    // eat the ')'
    getNextToken();
    return std::make_unique<CallExprAST>(idName, std::move(args), functionProtos);
}
            
// call when current token is a number. 
// creates a node then advances the lexer to the next token before returning it.
std::unique_ptr<ExprAST> Parser::parseNumberExpr()
{
    auto val = lexer.getNumValue();
    fprintf(stderr, "%f\n", val);
    auto result = std::make_unique<NumberExprAST>(val);
    getNextToken();
    return std::move(result);
}

// call when current token is an open parenthesis.
// checks for closing parenthesis, and eats both.
std::unique_ptr<ExprAST> Parser::parseParenExpr()
{
    getNextToken();
    auto v = parseExpression();
    if (!v)
    {
        return nullptr;
    }
    if (currentToken != ')')
    {
        logging::logErrorToken("expected ')'");
        return nullptr;
    }
    getNextToken();
    return v;
}

std::unique_ptr<ExprAST> Parser::parseIfExpr()
{
    // eat the if
    getNextToken();

    // parse the condition expression
    auto condition = parseExpression();
    if (!condition)
    {
        return nullptr;
    }

    if (currentToken != Lexer::Token::TOK_THEN)
    {
        logging::logErrorToken("Expected 'then'");
        return nullptr;
    }

    // eat the then
    getNextToken();

    // parse then then expression
    auto thenExpr = parseExpression();
    if (!thenExpr)
    {
        return nullptr;
    }

    if (currentToken != Lexer::Token::TOK_ELSE)
    {
        logging::logErrorToken("Expected 'else'");
        return nullptr;
    }

    // eat the else
    getNextToken();

    // parse the else expression
    auto elseExpr = parseExpression();
    if (!elseExpr)
    {
        return nullptr;
    }

    return std::make_unique<IfExprAST>(std::move(condition), std::move(thenExpr), 
                                       std::move(elseExpr));
}

std::unique_ptr<ExprAST> Parser::parseForExpr()
{
    // eat the for
    getNextToken();

    if (currentToken != Lexer::Token::TOK_IDENT)
    {
        logging::logErrorToken("Expected identifier after for.");
        return nullptr;
    }

    // eat the ident but save it
    std::string idName = lexer.getIdentStr();
    getNextToken(); 

    if (currentToken != '=')
    {
        logging::logErrorToken("Expected = after for.");
        return nullptr;
    }

    // eat =
    getNextToken();

    auto start = parseExpression();
    if (!start)
    {
        return nullptr;
    }

    if (currentToken != ',')
    {
        logging::logErrorToken("Expected ',' after for start value.");
        return nullptr;
    }

    getNextToken();

    auto end = parseExpression();
    if (!end)
    {
        return nullptr;
    }

    // the step value is optional
    std::unique_ptr<ExprAST> step;
    if (currentToken == ',')
    {
        getNextToken();
        step = parseExpression();        
        if (!step)
        {
            return nullptr;
        }
    }

    if (currentToken != Lexer::Token::TOK_IN)
    {
        logging::logErrorToken("Expected 'in' after for.");
        return nullptr;
    }

    getNextToken();

    auto body = parseExpression();
    if (!body)
    {
        return nullptr;
    }

    return std::make_unique<ForExprAST>(idName, std::move(start), std::move(end), 
                                        std::move(step), std::move(body));
}

int Parser::getTokPrecedence()
{
    if (!isascii(currentToken))
    {
        return -1;
    }

    int tokPrec = binopPrecedence[currentToken];
    return tokPrec <= 0 ? -1 : tokPrec;
}