#ifndef TOKEN_HPP
#define TOKEN_HPP
#include <string>
#include <iostream>
#include <vector>

class Token {
public: 
    enum Type { 
        WHITE_TOKEN,
        NEW_LINE,
        OPERATOR,
        KEYWORD,
        NUMBER,
        STRING,
        VARIABLE_REF,
        NIL,
        BRACKET,
        CURLY_BRACKET,
        PARENTHESES,
        BOOLEAN,
    };

    std::string mValue;
    Type mType;

};

class TokensStream {
public:
    using TokenIterator = std::vector<Token>::iterator;

    Token& Get();
    Token& Next(bool move = false);
    Token& Last(bool move = false);

    TokensStream() = delete;
    TokensStream(std::vector<Token> toks);

private:
    TokenIterator mTokIter;
    std::vector<Token> mTokens;
};


#endif
