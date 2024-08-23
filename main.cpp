#include <iostream>
#include <vector>
#include "Token.hpp"
#include "ASTCreator.hpp"

extern int   yylex();
extern char* yytext;
extern int   yyleng;
extern FILE * yyin;
extern std::vector<Token> __toks;

int main()
{
    std::string path = "/home/pnsv0/mlang/src/test.mlang";
    FILE *fp;
    fp = fopen(path.c_str(), "r");
    yyin = fp;
    yylex();
    std::cout << __toks.size() << std::endl;
    TokensStream tokStream(__toks);
    ASTCreator * creator = new ASTCreator(tokStream);
    IASTBase * ast = creator->Create();
}


