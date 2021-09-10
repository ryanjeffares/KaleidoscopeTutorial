#pragma once

#include <string>
#include <memory>
#include <vector>
#include <map>

#include "lexer.h"
#include "includellvm.h"
#include "logging.h"

namespace kaleidoscope
{
	namespace parser 
	{
        namespace ast
        {        
            struct LLVMModule
            {
                std::shared_ptr<llvm::LLVMContext> llvmContext;
                std::shared_ptr<llvm::IRBuilder<>> irBuilder;
                std::shared_ptr<llvm::Module> llvmModule;
                std::map<std::string, llvm::Value*> namedValues;

                LLVMModule()
                {
                    llvmContext = std::make_shared<llvm::LLVMContext>();
                    llvmModule = std::make_shared<llvm::Module>("Kaleidoscope JIT", *llvmContext);
                    irBuilder = std::make_shared<llvm::IRBuilder<>>(*llvmContext);
                }
            };

            // base class for all expression nodes
            class ExprAST
            {
            public:
                virtual ~ExprAST() = default;
                virtual llvm::Value* codeGen(LLVMModule&) = 0;
            };

            // expression class for numerical literals
            class NumberExprAST : public ExprAST
            {
            public:
                NumberExprAST(double v) : value(v) {}

                llvm::Value* codeGen(LLVMModule&) override;

            private:
                double value;
            };

            // expression class for referencing a variable
            class VariableExprAST : public ExprAST
            {
            public:
                VariableExprAST(const std::string& n) : name(n) {}

                llvm::Value* codeGen(LLVMModule&) override;  

            private:
                std::string name;
            };

            // expression class for binary operator
            class BinaryExprAST : public ExprAST
            {
            public:
                BinaryExprAST(char o, std::unique_ptr<ExprAST> l, std::unique_ptr<ExprAST> r)
                    : op(o), lhs(std::move(l)), rhs(std::move(r)) {}

                llvm::Value* codeGen(LLVMModule&) override;

            private:
                char op;
                std::unique_ptr<ExprAST> lhs, rhs;
            };

            // expression class for function calls
            class CallExprAST : public ExprAST
            {
            public:
                CallExprAST(const std::string& c, std::vector<std::unique_ptr<ExprAST>> a)
                    : callee(c), args(std::move(a)) {}

				llvm::Value* codeGen(LLVMModule&) override;

            private:
                std::string callee;
                std::vector<std::unique_ptr<ExprAST>> args;
            };

            // this class represents the prototype for a function,
            // which captures its name and its arguments' names
            class PrototypeAST
            {
            public:
                PrototypeAST(const std::string& n, std::vector<std::string> a)
                    : name(n), args(std::move(a)) {}

				llvm::Function* codeGen(LLVMModule&);

				const std::string& getName() const { return name; }

            private:
                std::string name;
                std::vector<std::string> args;
            };

            // this class represents a function definition itself
            class FunctionAST
            {
            public:
                FunctionAST(std::unique_ptr<PrototypeAST> p, std::unique_ptr<ExprAST> b)
                    : proto(std::move(p)), body(std::move(b)) {}

				llvm::Function* codeGen(LLVMModule&);
				
            private:
                std::unique_ptr<PrototypeAST> proto;
                std::unique_ptr<ExprAST> body;
            };
        } // namespace ast

		// the parser, which will build our abstract syntax tree.
		class Parser
		{
		public:            

			Parser();
			~Parser() = default;		

			void run();

            void printJitCode() { llvmModule.llvmModule->print(llvm::errs(), nullptr); }	

		private:						

			void mainLoop();			

			// provide a simple token buffer where we can look ahead one token
			// reads another token from the lexer and updates currentToken
			int getNextToken() { return currentToken = lexer.getToken(); }

			void handleDefinition();

			void handleExtern();

			void handleTopLevelExpression();	

			std::unique_ptr<ast::ExprAST> parsePrimary();

			std::unique_ptr<ast::PrototypeAST> parsePrototype();

			std::unique_ptr<ast::FunctionAST> parseDefinition();

			std::unique_ptr<ast::PrototypeAST> parseExtern();

			std::unique_ptr<ast::FunctionAST> parseTopLevelExpr();

			// gets the left hand side of a binary expression, then gets the right
			std::unique_ptr<ast::ExprAST> parseExpression();

			// gets the right hand side of a binary expression
			std::unique_ptr<ast::ExprAST> parseBinOpRhs(int exprPrec, std::unique_ptr<ast::ExprAST> lhs);

			// handles variable references and function calls
			std::unique_ptr<ast::ExprAST> parseIdentifierExpr();

			// call when current token is a number. 
			// creates a node then advances the lexer to the next token before returning it.
			std::unique_ptr<ast::ExprAST> parseNumberExpr();

			// call when current token is an open parenthesis.
			// checks for closing parenthesis, and eats both.
			std::unique_ptr<ast::ExprAST> parseParenExpr();

			int getTokPrecedence();			

		private:

			int currentToken;
			Lexer lexer;
            ast::LLVMModule llvmModule;

			std::map<char, int> binopPrecedence;
            
		};        
	}   // namespace parser   
}   // namespace kaleidoscope