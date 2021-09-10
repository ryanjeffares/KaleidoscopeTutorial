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
            l = llvmModule.irBuilder->CreateFCmpULT(l, r, "cmptmp");
            // convert bool 0/1 to double 0.0/1.0
            return llvmModule.irBuilder->CreateUIToFP(l, llvm::Type::getDoubleTy(*(llvmModule.llvmContext)), "booltmp");
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
                
// =================== Parser ===================

Parser::Parser()
{
    kaleidoscopeJit = exitOnError(llvm::orc::KaleidoscopeJIT::Create());
    binopPrecedence['<'] = 10;
    binopPrecedence['+'] = 20;
    binopPrecedence['-'] = 30;
    binopPrecedence['/'] = 40;
    binopPrecedence['*'] = 50;
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
            fprintf(stderr, "Evaluated to %f\n", fp());
                     
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
    auto result = std::make_unique<NumberExprAST>(lexer.getNumValue());
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

int Parser::getTokPrecedence()
{
    if (!isascii(currentToken))
    {
        return -1;
    }

    int tokPrec = binopPrecedence[currentToken];
    return tokPrec <= 0 ? -1 : tokPrec;
}