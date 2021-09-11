#pragma once

#include "includellvm.h"
#include "llvmmodule.h"

namespace kaleidoscope
{
    namespace ast
    {
        // base class for all expression nodes
        class ExprAST
        {
        public:
            virtual ~ExprAST() = default;
            virtual llvm::Value* codeGen(kaleidoscope::LLVMTools&) = 0;
        };

        // expression class for numerical literals
        class NumberExprAST : public ExprAST
        {
        public:
            NumberExprAST(double v) : value(v) {}

            llvm::Value* codeGen(kaleidoscope::LLVMTools&) override;

        private:
            double value;
        };

        // expression class for referencing a variable
        class VariableExprAST : public ExprAST
        {
        public:
            VariableExprAST(const std::string& n) : name(n) {}

            llvm::Value* codeGen(kaleidoscope::LLVMTools&) override;  

        private:
            std::string name;
        };

        // this class represents the prototype for a function,
        // which captures its name and its arguments' names
        class PrototypeAST
        {
        public:
            PrototypeAST(const std::string& n, std::vector<std::string> a,
                         bool isOp = false, unsigned prec = 0)
                         : name(n), args(std::move(a)) {}

            llvm::Function* codeGen(kaleidoscope::LLVMTools&);

            const std::string& getName() const { return name; }

            bool isUnaryOp() const { return isOperator && args.size() == 1; }
            bool isBinaryOp() const { return isOperator && args.size() == 2; }
            unsigned getBinaryPrecedence() const { return precedence; }

            char getOperatorName() const
            {
                assert(isUnaryOp() || isBinaryOp());
                return name.back();
            }

        private:
            std::string name;
            std::vector<std::string> args;
            bool isOperator;
            unsigned precedence;
        };

        // expression class for binary operator
        class BinaryExprAST : public ExprAST
        {
        public:
            BinaryExprAST(char o, std::unique_ptr<ExprAST> l, std::unique_ptr<ExprAST> r,
                          std::map<std::string, std::unique_ptr<PrototypeAST>>& p)
                          : op(o), lhs(std::move(l)), rhs(std::move(r)), protos(p) {}

            llvm::Value* codeGen(kaleidoscope::LLVMTools&) override;

        private:
            char op;
            std::unique_ptr<ExprAST> lhs, rhs;
            std::map<std::string, std::unique_ptr<PrototypeAST>>& protos;
        };        

        class UnaryExprAST : public ExprAST
        {
        public:
            UnaryExprAST(char oc, std::unique_ptr<ExprAST> op,
                         std::map<std::string, std::unique_ptr<PrototypeAST>>& p)
                         : opcode(oc), operand(std::move(op)), protos(p) {}

            llvm::Value* codeGen(LLVMTools&) override;

        private:
            char opcode;
            std::unique_ptr<ExprAST> operand;
            std::map<std::string, std::unique_ptr<PrototypeAST>>& protos;
        };        

        // expression class for function calls
        // needs to be given a reference to Parser's prototype lookup on construction
        // so that it can call findFunction later - this is BAD and i should find a cleaner way
        class CallExprAST : public ExprAST
        {
        public:
            CallExprAST(const std::string& c, std::vector<std::unique_ptr<ExprAST>> a, 
                std::map<std::string, std::unique_ptr<PrototypeAST>>& map)
                : callee(c), args(std::move(a)), protos(map) {}

            llvm::Value* codeGen(kaleidoscope::LLVMTools&) override;            

        private:
            std::string callee;
            std::vector<std::unique_ptr<ExprAST>> args;
            std::map<std::string, std::unique_ptr<PrototypeAST>>& protos;
        };

        // this class represents a function definition itself
        class FunctionAST
        {
        public:
            FunctionAST(std::unique_ptr<PrototypeAST> p, std::unique_ptr<ExprAST> b,
                        std::map<char, int>& bp)
                        : proto(std::move(p)), 
                          body(std::move(b)), 
                          binopPrecedence(bp) {}

            llvm::Function* codeGen(kaleidoscope::LLVMTools&, std::map<std::string, std::unique_ptr<PrototypeAST>>&);
            
        private:
            std::unique_ptr<PrototypeAST> proto;
            std::unique_ptr<ExprAST> body;
            std::map<char, int>& binopPrecedence;
        };

        class IfExprAST : public ExprAST
        {
        public:
            IfExprAST(std::unique_ptr<ExprAST> cond, std::unique_ptr<ExprAST> then,
                      std::unique_ptr<ExprAST> els) 
                      : condition(std::move(cond)), thenExpr(std::move(then)), elseExpr(std::move(els)) {}

            llvm::Value* codeGen(kaleidoscope::LLVMTools&) override;

        private:
            std::unique_ptr<ExprAST> condition, thenExpr, elseExpr;
        };

        class ForExprAST : public ExprAST
        {
        public:
            ForExprAST(const std::string& name, 
                       std::unique_ptr<ExprAST> strt,
                       std::unique_ptr<ExprAST> en,
                       std::unique_ptr<ExprAST> stp,
                       std::unique_ptr<ExprAST> bd)
                       : varName(std::move(name)), start(std::move(strt)),
                         end(std::move(en)), step(std::move(stp)), 
                         body(std::move(bd)) {}

            llvm::Value* codeGen(kaleidoscope::LLVMTools&) override;
        private:
            std::string varName;
            std::unique_ptr<ExprAST> start, end, step, body;
        }; 

        static llvm::Function* findFunction(const std::string& name,
                                            kaleidoscope::LLVMTools& llvmTools,
                                            std::map<std::string, std::unique_ptr<kaleidoscope::ast::PrototypeAST>>& protos)
        {
            if (auto f = llvmTools.llvmModule->getFunction(name))
            {
                return f;
            }

            auto it = protos.find(name);
            if (it != protos.end())
            {
                return it->second->codeGen(llvmTools);
            }

            return nullptr;
        }
    } // namespace ast    
} // namespace kaleidoscope