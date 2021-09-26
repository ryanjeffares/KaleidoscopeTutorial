#pragma once

#include <iostream>

#include "includellvm.h"
#include "llvmtools.h"
#include "lexer.h"

namespace kaleidoscope
{
    namespace ast
    {
        namespace detail
        {
            static llvm::raw_ostream& indent(llvm::raw_ostream& out, int size)
            {
                return out << std::string(size, ' ');
            }
        } // namespace detail        

        // base class for all expression nodes
        class ExprAST
        {
        public:        
            ExprAST(kaleidoscope::SourceLocation loc = kaleidoscope::Lexer::currentLocation)
                : location(loc) {}

            virtual ~ExprAST() = default;

            virtual llvm::Value* codeGen(kaleidoscope::LLVMTools&) = 0;

            int getLine() const { return location.line; }
            int getColumn() const { return location.column; }

            virtual llvm::raw_ostream& dump(llvm::raw_ostream& out, int ind)
            {
                return out << ':' << getLine() << ':' << getColumn() << '\n';
            }

        private:
            kaleidoscope::SourceLocation location;
        };

        // expression class for numerical literals
        class NumberExprAST : public ExprAST
        {
        public:
            NumberExprAST(double v) : value(v) {}

            llvm::Value* codeGen(kaleidoscope::LLVMTools&) override;

            llvm::raw_ostream& dump(llvm::raw_ostream& out, int ind) override 
            {
                return ExprAST::dump(out << value, ind);
            }

        private:
            double value;
        };

        // expression class for var/in 
        class VarExprAST : public ExprAST
        {
        public:
            VarExprAST(std::vector<std::pair<std::string, std::unique_ptr<ExprAST>>> vNames,
                       std::unique_ptr<ExprAST> b) 
                      : varNames(std::move(vNames)), body(std::move(b)) {}
                
            llvm::Value* codeGen(kaleidoscope::LLVMTools&) override;

            llvm::raw_ostream& dump(llvm::raw_ostream& out, int ind) override
            {
                ExprAST::dump(out << "var", ind);
                for (const auto& namedVar : varNames)
                {
                    namedVar.second->dump(
                        detail::indent(out, ind) << namedVar.first << ':', ind + 1
                    );                    
                }
                body->dump(detail::indent(out, ind) << "Body:", ind + 1);
                return out;
            }
            
        private:
            std::vector<std::pair<std::string, std::unique_ptr<ExprAST>>> varNames;
            std::unique_ptr<ExprAST> body;
        };

        // expression class for referencing a variable
        class VariableExprAST : public ExprAST
        {
        public:
            VariableExprAST(kaleidoscope::SourceLocation location, 
                            const std::string& n) 
                            : ExprAST(location),
                              name(n) {}

            llvm::Value* codeGen(kaleidoscope::LLVMTools&) override;

            const std::string& getName() const { return name; }

            llvm::raw_ostream& dump(llvm::raw_ostream& out, int ind) override
            {
                return ExprAST::dump(out << name, ind);   
            }

        private:
            std::string name;
        };

        // this class represents the prototype for a function,
        // which captures its name and its arguments' names
        class PrototypeAST
        {
        public:
            PrototypeAST(kaleidoscope::SourceLocation location,
                         const std::string& n, 
                         std::vector<std::string> a,
                         bool isOp = false, 
                         unsigned prec = 0)
                         : line(location.line),
                           name(n), 
                           args(std::move(a)), 
                           isOperator(isOp), 
                           precedence(prec) {}

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

            int getLine() const { return line; }

        private:
            std::string name;
            std::vector<std::string> args;
            bool isOperator;
            unsigned precedence;
            int line;
        };

        // expression class for binary operator
        class BinaryExprAST : public ExprAST
        {
        public:
            BinaryExprAST(kaleidoscope::SourceLocation location,
                          char o, 
                          std::unique_ptr<ExprAST> l, 
                          std::unique_ptr<ExprAST> r,
                          std::map<std::string, std::unique_ptr<PrototypeAST>>& p)
                          : ExprAST(location),
                            op(o), 
                            lhs(std::move(l)), 
                            rhs(std::move(r)), 
                            protos(p) {}

            llvm::Value* codeGen(kaleidoscope::LLVMTools&) override;

            llvm::raw_ostream& dump(llvm::raw_ostream& out, int ind) override
            {
                ExprAST::dump(out << "binary" << op, ind);
                lhs->dump(detail::indent(out, ind) << "LHS:", ind + 1);
                rhs->dump(detail::indent(out, ind) << "RHS:", ind + 1);
                return out;
            }

        private:
            char op;
            std::unique_ptr<ExprAST> lhs, rhs;
            std::map<std::string, std::unique_ptr<PrototypeAST>>& protos;
        };        

        class UnaryExprAST : public ExprAST
        {
        public:
            UnaryExprAST(char oc, 
                         std::unique_ptr<ExprAST> op,
                         std::map<std::string, std::unique_ptr<PrototypeAST>>& p)
                         : opcode(oc), 
                           operand(std::move(op)), 
                           protos(p) {}

            llvm::Value* codeGen(LLVMTools&) override;

            llvm::raw_ostream& dump(llvm::raw_ostream& out, int ind) override
            {
                ExprAST::dump(out << "unary" << opcode, ind);
                operand->dump(out, ind + 1);
                return out;
            }

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
            CallExprAST(kaleidoscope::SourceLocation location,
                        const std::string& c, 
                        std::vector<std::unique_ptr<ExprAST>> a, 
                        std::map<std::string, std::unique_ptr<PrototypeAST>>& map)
                        : ExprAST(location),
                          callee(c), 
                          args(std::move(a)), 
                          protos(map) {}

            llvm::Value* codeGen(kaleidoscope::LLVMTools&) override;            

            llvm::raw_ostream& dump(llvm::raw_ostream& out, int ind) override 
            {
                ExprAST::dump(out << "call" << callee, ind);
                for (const auto& arg : args)
                {
                    arg->dump(detail::indent(out, ind + 1), ind + 1);
                }
                return out;
            }

        private:
            std::string callee;
            std::vector<std::unique_ptr<ExprAST>> args;
            std::map<std::string, std::unique_ptr<PrototypeAST>>& protos;
        };

        // this class represents a function definition itself
        class FunctionAST
        {
        public:
            FunctionAST(std::unique_ptr<PrototypeAST> p, 
                        std::unique_ptr<ExprAST> b,
                        std::map<char, int>& bp)
                        : proto(std::move(p)), 
                          body(std::move(b)), 
                          binopPrecedence(bp) {}

            llvm::Function* codeGen(kaleidoscope::LLVMTools&, std::map<std::string, std::unique_ptr<PrototypeAST>>&);
            
            llvm::raw_ostream& dump(llvm::raw_ostream& out, int ind)
            {
                detail::indent(out, ind) << "FunctionAST\n";
                ++ind;
                detail::indent(out, ind) << "Body:";
                return body ? body->dump(out, ind) : out << "null\n";
            }

        private:
            std::unique_ptr<PrototypeAST> proto;
            std::unique_ptr<ExprAST> body;
            std::map<char, int>& binopPrecedence;
        };

        class IfExprAST : public ExprAST
        {
        public:
            IfExprAST(kaleidoscope::SourceLocation location,
                      std::unique_ptr<ExprAST> cond, 
                      std::unique_ptr<ExprAST> then,
                      std::unique_ptr<ExprAST> els) 
                      : ExprAST(location),
                        condition(std::move(cond)), 
                        thenExpr(std::move(then)), 
                        elseExpr(std::move(els)) {}

            llvm::Value* codeGen(kaleidoscope::LLVMTools&) override;

            llvm::raw_ostream& dump(llvm::raw_ostream& out, int ind) override 
            {
                ExprAST::dump(out << "if", ind);
                condition->dump(detail::indent(out, ind) << "Cond:", ind + 1);
                thenExpr->dump(detail::indent(out, ind) << "Then:", ind + 1);
                elseExpr->dump(detail::indent(out, ind) << "Else:", ind + 1);
                return out;
            }

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
                       : varName(std::move(name)), 
                         start(std::move(strt)),
                         end(std::move(en)), 
                         step(std::move(stp)), 
                         body(std::move(bd)) {}

            llvm::Value* codeGen(kaleidoscope::LLVMTools&) override;

            llvm::raw_ostream& dump(llvm::raw_ostream& out, int ind) override 
            {
                ExprAST::dump(out << "for", ind);
                start->dump(detail::indent(out, ind) << "Cond:", ind + 1);
                end->dump(detail::indent(out, ind) << "End:", ind + 1);
                step->dump(detail::indent(out, ind) << "Step:", ind + 1);
                body->dump(detail::indent(out, ind) << "Body:", ind + 1);
                return out;
            }

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

        static llvm::AllocaInst* createEntryBlockAlloca(llvm::Function* func,
                                                        const std::string& name,
                                                        kaleidoscope::LLVMTools& llvmTools)
        {
            llvm::IRBuilder<> tempBuilder(&func->getEntryBlock(),
                                          func->getEntryBlock().begin()
            );
            return tempBuilder.CreateAlloca(
                llvm::Type::getDoubleTy(*(llvmTools.llvmContext)),
                0, 
                name.c_str()
            );   
        }
    } // namespace ast    
} // namespace kaleidoscope
