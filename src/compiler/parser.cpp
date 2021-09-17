#include "parser.h"

using namespace kaleidoscope::parser;
using namespace kaleidoscope::ast;

Parser::Parser()
{
    kaleidoscopeJit = exitOnError(llvm::orc::KaleidoscopeJIT::Create());    
    binopPrecedence['<'] = 10;
    binopPrecedence['+'] = 20;
    binopPrecedence['-'] = 20;    
    binopPrecedence['*'] = 40;
}	

void Parser::run()
{
    getNextToken();    
    initModuleAndPassManager();
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
    llvmTools.doInit(kaleidoscopeJit.get());
}

void Parser::handleDefinition()
{
    if (auto funcAst = parseDefinition())
    {
        if (auto funcIr = funcAst->codeGen(llvmTools, functionProtos))
        {
            fprintf(stderr, "Read function definition: \n");
            funcIr->print(llvm::errs());
            fprintf(stderr, "\n");
            exitOnError(kaleidoscopeJit->addModule(
                llvm::orc::ThreadSafeModule(std::move(llvmTools.llvmModule), 
                std::move(llvmTools.llvmContext)))
            );
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
        if (auto funcIr = funcAst->codeGen(llvmTools))
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
        if (funcAst->codeGen(llvmTools, functionProtos))
        {
            // jit the module containing the anonymous expression,
            // keeping a handle on it so we can free it later
            auto resourceTracker = kaleidoscopeJit->getMainJITDylib().createResourceTracker();
            
            auto tsm = llvm::orc::ThreadSafeModule(std::move(llvmTools.llvmModule), 
                std::move(llvmTools.llvmContext));
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
    std::string funcName;
    unsigned kind = 0; // 0=ident, 1=unary, 2=binary
    unsigned binaryPrecedence = 30;

    switch (currentToken)
    {
        case Lexer::Token::TOK_IDENT:
            funcName = lexer.getIdentStr();
            kind = 0;
            getNextToken();
            break;
        case Lexer::Token::TOK_BINARY:
            getNextToken();
            if (!isascii(currentToken))
            {
                logging::logErrorProto("Expected binary operator.");
                return nullptr;
            }
            funcName = "binary";
            funcName += (char)currentToken;
            kind = 2;
            getNextToken();

            // read precedence if present
            if (currentToken == Lexer::Token::TOK_NUM)
            {
                auto num = lexer.getNumValue();
                if (num < 1 || num > 100)
                {
                    logging::logErrorProto("Invalid precedence, must be 1..100.");
                    return nullptr;
                }
                binaryPrecedence = (unsigned)num;
                getNextToken();
            }
            break;
        case Lexer::Token::TOK_UNARY:
            getNextToken();
            if (!isascii(currentToken))
            {
                logging::logErrorProto("Expected unary operator.");
                return nullptr;
            }
            funcName = "unary";
            funcName += (char)currentToken;
            kind = 1;
            getNextToken();
            break;
        default:
            logging::logErrorProto("Expected function name in prototype.");
            return nullptr;
    }

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

    if (kind && argNames.size() != kind)
    {
        logging::logErrorProto("Invalid number of operands for operator.");
        return nullptr;
    }

    return std::make_unique<PrototypeAST>(
        funcName, argNames, kind != 0, binaryPrecedence
    );
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
        return std::make_unique<FunctionAST>(
            std::move(proto), std::move(e), binopPrecedence
        );
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
        auto proto = std::make_unique<PrototypeAST>(
            "__anon_expr", std::vector<std::string>()
        );
        return std::make_unique<FunctionAST>(
            std::move(proto), std::move(e), binopPrecedence
        );
    }
    return nullptr;
}

// gets the left hand side of a binary expression, then gets the right
std::unique_ptr<ExprAST> Parser::parseExpression()
{
    auto lhs = parseUnary();
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

        auto rhs = parseUnary();
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
        lhs = std::make_unique<BinaryExprAST>(
            binOp, std::move(lhs), std::move(rhs), functionProtos
        );
    }
}

std::unique_ptr<ExprAST> Parser::parseUnary()
{
    // if the current token is not an operator, it must be a primary expression
    if (!isascii(currentToken) || currentToken == '(' || currentToken == ',')
    {
        return parsePrimary();
    }

    // read the unary operator
    int opcode = currentToken;
    getNextToken();
    if (auto operand = parseUnary())
    {
        return std::make_unique<UnaryExprAST>(opcode, std::move(operand), functionProtos);
    }

    return nullptr;
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