#include "parser.h"

using namespace kaleidoscope::parser;
using namespace kaleidoscope::parser::ast;

// =================== AST ===================

llvm::Value* NumberExprAST::codeGen()
{
    return llvm::ConstantFP::get(*llvmContext, llvm::APFloat(value));
}

llvm::Value* VariableExprAST::codeGen()
{
    auto v = namedValues[name];
    if (!v)
    {
        logging::logErrorValue("Unknown variable name.");                        
    }
    return v;
} 

llvm::Value* BinaryExprAST::codeGen() 
{
    auto l = lhs->codeGen();
    auto r = rhs->codeGen();

    if (!l || !r)
    {
        return nullptr;
    }

    switch (op)
    {
        case '+':
            return irBuilder->CreateFAdd(l, r, "addtmp");                        
        case '-':
            return irBuilder->CreateFSub(l, r, "subtmp");
        case '/':
            return irBuilder->CreateFDiv(l, r, "divtmp");
        case '*':
            return irBuilder->CreateFMul(l, r, "multmp");
        case '<':
            l = irBuilder->CreateFCmpULT(l, r, "cmptmp");
            // convert bool 0/1 to double 0.0/1.0
            return irBuilder->CreateUIToFP(l, llvm::Type::getDoubleTy(*llvmContext), "booltmp");
        default:
            logging::logErrorValue("Invalid binary operator.");
            return nullptr;                            
    }
}

llvm::Value* CallExprAST::codeGen()
{
    auto calleeFunc = llvmModule->getFunction(callee);
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
        argsVec.push_back(args[i]->codeGen());
        if (!argsVec.back())
        {
            return nullptr;
        }
    }

    return irBuilder->CreateCall(calleeFunc, argsVec, "calltmp");
}

llvm::Function* PrototypeAST::codeGen()
{
    std::vector<llvm::Type*> doubles(args.size(), llvm::Type::getDoubleTy(*llvmContext));
    auto fType = llvm::FunctionType::get(llvm::Type::getDoubleTy(*llvmContext), doubles, false);
    auto func = llvm::Function::Create(fType, llvm::Function::ExternalLinkage, name, llvmModule.get());

    unsigned index = 0;
    for (auto& arg : func->args())
    {
        arg.setName(args[index++]);
    }

    return func;
}

llvm::Function* FunctionAST::codeGen()
{
    // first check for existing function from a previous extern declaration
    auto func = llvmModule->getFunction(proto->getName());

    if (!func)
    {
        func = proto->codeGen();
    }

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
    auto block = llvm::BasicBlock::Create(*llvmContext, "entry", func);
    irBuilder->SetInsertPoint(block);

    // clear the map and record the function arguments there
    namedValues.clear();
    for (auto& arg : func->args())
    {
        namedValues[std::string(arg.getName())] = &arg;
    }

    if (auto returnValue = body->codeGen())
    {
        // finish off the function and validate it
        irBuilder->CreateRet(returnValue);
        llvm::verifyFunction(*func);
        return func;
    }

    // erase the function from memory if there was a problem
    func->eraseFromParent();
    return func;
}
                
// =================== Parser ===================

Parser::Parser()
{
    binopPrecedence['<'] = 10;
    binopPrecedence['+'] = 20;
    binopPrecedence['-'] = 30;
    binopPrecedence['/'] = 40;
    binopPrecedence['*'] = 50;
}	

void Parser::run()
{
    getNextToken();
    initialiseModule();
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

void Parser::initialiseModule()
{
    ast::llvmContext = std::make_unique<llvm::LLVMContext>();
    ast::llvmModule = std::make_unique<llvm::Module>("Kaleidoscope JIT", *(ast::llvmContext));
    ast::irBuilder = std::make_unique<llvm::IRBuilder<>>(*(ast::llvmContext));
}

void Parser::handleDefinition()
{
    if (auto funcAst = parseDefinition())
    {
        if (auto funcIr = funcAst->codeGen())
        {
            fprintf(stderr, "Read function definition: \n");
            funcIr->print(llvm::errs());
            fprintf(stderr, "\n");
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
        if (auto funcIr = funcAst->codeGen())
        {
            fprintf(stderr, "Read extern: \n");
            funcIr->print(llvm::errs());
            fprintf(stderr, "\n");
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
        if (auto funcIr = funcAst->codeGen())
        {
            fprintf(stderr, "Read a top level expression: \n");
            funcIr->print(llvm::errs());
            fprintf(stderr, "\n");
            funcIr->eraseFromParent();
        }
    }
    else 
    {
        getNextToken();
    }
}	

std::unique_ptr<ast::ExprAST> Parser::parsePrimary()
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

std::unique_ptr<ast::PrototypeAST> Parser::parsePrototype()
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
    return std::make_unique<ast::PrototypeAST>(funcName, std::move(argNames));
}
            
std::unique_ptr<ast::FunctionAST> Parser::parseDefinition()
{
    getNextToken();

    auto proto = parsePrototype();

    if (!proto)
    {
        return nullptr;
    }

    if (auto e = parseExpression())
    {
        return std::make_unique<ast::FunctionAST>(std::move(proto), std::move(e));
    }

    return nullptr;
}
            
std::unique_ptr<ast::PrototypeAST> Parser::parseExtern()
{
    getNextToken();
    return parsePrototype();    
}

std::unique_ptr<ast::FunctionAST> Parser::parseTopLevelExpr()
{
    if (auto e = parseExpression())
    {
        auto proto = std::make_unique<ast::PrototypeAST>("__anon_expr", std::vector<std::string>());
        return std::make_unique<ast::FunctionAST>(std::move(proto), std::move(e));
    }
    return nullptr;
}

// gets the left hand side of a binary expression, then gets the right
std::unique_ptr<ast::ExprAST> Parser::parseExpression()
{
    auto lhs = parsePrimary();
    if (!lhs)
    {
        return nullptr;
    }
    return parseBinOpRhs(0, std::move(lhs));
}

// gets the right hand side of a binary expression
std::unique_ptr<ast::ExprAST> Parser::parseBinOpRhs(int exprPrec, std::unique_ptr<ast::ExprAST> lhs)
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
        lhs = std::make_unique<ast::BinaryExprAST>(binOp, std::move(lhs), std::move(rhs));
    }
}
            
// handles variable references and function calls
std::unique_ptr<ast::ExprAST> Parser::parseIdentifierExpr()
{
    std::string idName = lexer.getIdentStr();
    // eat the identifier
    getNextToken();

    // simple variable reference
    if (currentToken != '(')
    {
        return std::make_unique<ast::VariableExprAST>(idName);
    }

    // eat the token
    getNextToken();
    std::vector<std::unique_ptr<ast::ExprAST>> args;
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
    return std::make_unique<ast::CallExprAST>(idName, std::move(args));
}
            
// call when current token is a number. 
// creates a node then advances the lexer to the next token before returning it.
std::unique_ptr<ast::ExprAST> Parser::parseNumberExpr()
{
    auto result = std::make_unique<ast::NumberExprAST>(lexer.getNumValue());
    getNextToken();
    return std::move(result);
}

// call when current token is an open parenthesis.
// checks for closing parenthesis, and eats both.
std::unique_ptr<ast::ExprAST> Parser::parseParenExpr()
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