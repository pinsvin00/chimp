#ifndef AST_CREATOR_HPP
#define AST_CREATOR_HPP

#include <iostream>
#include <set>
#include "Token.hpp"
#include "AST.hpp"
#include <memory>
#include <map>
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
#include <algorithm>


class Context { 
    std::unique_ptr<llvm::LLVMContext> TheContext;
    std::unique_ptr<llvm::Module> TheModule;
    std::unique_ptr<llvm::IRBuilder<>> Builder;
    std::map<std::string, llvm::Value *> NamedValues;
};

class ASTCreator {
public:
    IASTBase * Create();
    IASTBase * TryReadNext();
    IASTBase * TryReadLast();
    ASTCreator(TokensStream& tokStream) : mTokensStream(tokStream) {
        mAstRoot = nullptr;
    }
    bool IsTwoArgOperator(const Token& tok) const;

private:
    Context* mAstLLVMContext;
    TokensStream& mTokensStream; 
    AST* mAstRoot;
};

#endif