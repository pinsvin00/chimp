#include "Token.hpp"

TokensStream::TokensStream(std::vector<Token> tokens)
{
    mTokens = std::move(tokens);
    mTokIter = mTokens.begin();
}

Token& TokensStream::Get()
{
    return *mTokIter;
}

Token& TokensStream::Next(bool move /*=false*/)
{
    auto& tok  = *mTokIter;
    if(mTokens.end() != mTokIter && move)
    {
        mTokIter++;
    }
    return tok;
}

Token& TokensStream::Last(bool move /*=false*/)
{
    auto& tok  = *mTokIter;
    if(mTokens.begin() != mTokIter && move)
    {
        mTokIter--;
    }
    return tok;
}