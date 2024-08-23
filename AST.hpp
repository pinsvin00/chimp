#include <iostream>
#include <vector>

#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"

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
    virtual void codeGen() = 0;
};


class NumberAST : public IASTBase {
private:
    bool isFloatingPoint;
    std::string value;
    virtual void codeGen() override
    {

    }
};
class StringAST : public IASTBase { 
    std::string value;
    virtual void codeGen() override
    {

    }
};

class OperatorAST : public IASTBase {
public:
    OperatorAST() = default;
    IASTBase* mLeft;
    IASTBase* mRight;
    char mOp;
    virtual void codeGen() override
    {
        // llvm::Value *L = mLeft->codegen();
        // llvm::Value *R = mRight->codegen();
        // if (!L || !R)
        //     return nullptr;

        // switch (mOp) {
        // case '+':
        //     return Builder->CreateFAdd(L, R, "addtmp");
        // case '-':
        //     return Builder->CreateFSub(L, R, "subtmp");
        // case '*':
        //     return Builder->CreateFMul(L, R, "multmp");
        // case '<':
        //     L = Builder->CreateFCmpULT(L, R, "cmptmp");
        //     // Convert bool 0/1 to double 0.0 or 1.0
        //     return Builder->CreateUIToFP(L, Type::getDoubleTy(*TheContext), "booltmp");
        // default:
        //     return LogErrorV("invalid binary operator");
        // }
    }
};


// class KeywordAST : public IASTBase { 
    
// };