#include <iostream>
#include <vector>
#include "ASTContext.hpp"

class AST {

};

class IASTBase {
public:
    IASTBase(){
        mAstType = UNDEFINED;
    }
    enum Type {
        UNDEFINED, 
        NUMBER,
        STRING,
        OPERATOR,
    };
    Type mAstType;
    std::string mDebugValue;
    virtual llvm::Value* CodeGen(Context * ctx) = 0;
};


class NumberAST : public IASTBase {
private:
    bool isFloatingPoint;
    std::string value;
    virtual llvm::Value* CodeGen(Context * ctx) override
    {
        return llvm::ConstantFP::get(*ctx->TheContext, llvm::APFloat(1.0f));
    }
};
class StringAST : public IASTBase { 
    std::string value;
    virtual llvm::Value* CodeGen(Context * ctx) override
    {
        return nullptr;
        //return llvm::ConstantFP::get(*ctx->TheContext, llvm::StringLiteral(value));
    }
};

class OperatorAST : public IASTBase {
public:
    OperatorAST() = default;
    IASTBase* mLeft;
    IASTBase* mRight;
    char mOp;
    virtual llvm::Value* CodeGen(Context * ctx) override
    {
        llvm::Value *L = mLeft->CodeGen(ctx);
        llvm::Value *R = mRight->CodeGen(ctx);
        if (!L || !R)
            return nullptr;

        switch (mOp) {
        case '+':
            return ctx->Builder->CreateFAdd(L, R, "addtmp");
        case '-':
            return ctx->Builder->CreateFSub(L, R, "subtmp");
        case '*':
            return ctx->Builder->CreateFMul(L, R, "multmp");
        case '<':
            L = ctx->Builder->CreateFCmpULT(L, R, "cmptmp");
            // Convert bool 0/1 to double 0.0 or 1.0
            return ctx->Builder->CreateUIToFP(L, llvm::Type::getDoubleTy(*ctx->TheContext), "booltmp");
        default:
            return nullptr;
        }
    }
};


// class KeywordAST : public IASTBase { 
    
// };