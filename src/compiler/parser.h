#pragma once

#include <string>
#include <memory>
#include <vector>
#include <map>

#include "lexer.h"
#include "logging.h"
#include "llvmmodule.h"
#include "ast.h"
#include "../../include/KaleidoscopeJIT.h"

namespace kaleidoscope
{
	namespace parser 
	{             
		// the parser, which will build our abstract syntax tree.
		class Parser
		{
		public:            

			Parser();
			~Parser() = default;		

			void run();

            void printJitCode() { llvmTools.llvmModule->print(llvm::errs(), nullptr); }

            void initModuleAndPassManager();                               

        public:

            llvm::ExitOnError exitOnError;   
            std::unique_ptr<llvm::orc::KaleidoscopeJIT> kaleidoscopeJit;
                        
		private:						                        

            // the main loop that reads from standard input
			void mainLoop();			

			// provide a simple token buffer where we can look ahead one token
			// reads another token from the lexer and updates currentToken
			int getNextToken() { return currentToken = lexer.getToken(); }

            // handlers for the various types of expressions, called from mainLoop()
			void handleDefinition();

			void handleExtern();

			void handleTopLevelExpression();	

            // begins parsing a binary expression, starts recursion
			std::unique_ptr<kaleidoscope::ast::ExprAST> parsePrimary();

            // parses the definition of a function, ie its name and args
			std::unique_ptr<kaleidoscope::ast::PrototypeAST> parsePrototype();

            // parses the body of a function
			std::unique_ptr<kaleidoscope::ast::FunctionAST> parseDefinition();

            // parses a prototype of an extern function
			std::unique_ptr<kaleidoscope::ast::PrototypeAST> parseExtern();

            // parses a top level expression, ie a call to a function or top level arithmetic
			std::unique_ptr<kaleidoscope::ast::FunctionAST> parseTopLevelExpr();

			// gets the left hand side of a binary expression, then gets the right
			std::unique_ptr<kaleidoscope::ast::ExprAST> parseExpression();

			// gets the right hand side of a binary expression
			std::unique_ptr<kaleidoscope::ast::ExprAST> parseBinOpRhs(int exprPrec, std::unique_ptr<kaleidoscope::ast::ExprAST> lhs);

			// handles variable references and function calls
			std::unique_ptr<kaleidoscope::ast::ExprAST> parseIdentifierExpr();

			// call when current token is a number. 
			// creates a node then advances the lexer to the next token before returning it.
			std::unique_ptr<kaleidoscope::ast::ExprAST> parseNumberExpr();

			// call when current token is an open parenthesis.
			// checks for closing parenthesis, and eats both.
			std::unique_ptr<kaleidoscope::ast::ExprAST> parseParenExpr();

            // parses an if/then/else expression
            std::unique_ptr<kaleidoscope::ast::ExprAST> parseIfExpr();

            // parses a for loop expression
            std::unique_ptr<kaleidoscope::ast::ExprAST> parseForExpr();

			int getTokPrecedence();			

		private:

			int currentToken;
			Lexer lexer;            
            kaleidoscope::LLVMTools llvmTools;
			std::map<char, int> binopPrecedence;
            std::map<std::string, std::unique_ptr<kaleidoscope::ast::PrototypeAST>> functionProtos;
                      
		};
	}   // namespace parser   
}   // namespace kaleidoscope