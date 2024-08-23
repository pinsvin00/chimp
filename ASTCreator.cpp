#include "ASTCreator.hpp"

IASTBase* ASTCreator::Create()
{
    Token & tok  = mTokensStream.Next();
    if(IsTwoArgOperator(tok))
    {
        OperatorAST * op = new OperatorAST();
        op->mLeft = TryReadNext();
        op->mRight = TryReadNext();
    }

    return nullptr;
}

IASTBase* ASTCreator::TryReadLast()
{
    return nullptr;
}

IASTBase* ASTCreator::TryReadNext()
{
    return nullptr;
}

bool ASTCreator::IsTwoArgOperator(const Token& tok) const
{
    std::set<std::string> twoArgOperators{"+", "-", "/", "*"};
    return tok.mType == Token::Type::OPERATOR && twoArgOperators.contains(tok.mValue);
}