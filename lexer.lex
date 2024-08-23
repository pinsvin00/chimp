/* Declaring two counters one for number
of lines other for number of characters
Taken from master/poplang/lexer.lex
 */
%{
#include <vector>
#include <cstdlib>
#include <iostream>
#include "Token.hpp"

std::vector<Token> __toks;

char* _substr(const char *src, int m, int n)
{
    // get the length of the destination string
    int len = n - m;
 
    // allocate (len + 1) chars for destination (+1 for extra null character)
    char *dest = (char*)malloc(sizeof(char) * (len + 1));
 
    // extracts characters between m'th and n'th index from source string
    // and copy them into the destination string
    for (int i = m; i < n && (*(src + i) != '\0'); i++)
    {
        *dest = *(src + i);
        dest++;
    }
 
    // null-terminate the destination string
    *dest = '\0';
 
    // return the destination string
    return dest - len;
}

%}

NUMBER    [0-9][0-9]*
EXPONENT [eE](\+\-)?[1-9]\d*
STRING   \"([^\\\"]|\\.)*\"
VARIABLE [a-zA-Z_]+[a-zA-Z_$0-9]*

%%
{NUMBER}+{EXPONENT} {
    Token tok;
    tok.mValue = strdup(yytext);
    tok.mType = Token::Type::NUMBER;
    __toks.push_back(tok);
}

{NUMBER} {
    Token tok;
    tok.mValue = strdup(yytext);
    tok.mType = Token::Type::NUMBER;
    __toks.push_back(tok);
}

{STRING} {
    Token tok;
    size_t strl = strlen(yytext);
    tok.mValue = _substr(yytext, 1, strl-1);
    tok.mType = Token::Type::STRING;

    __toks.push_back(tok);
}

"null" {
    Token tok;
    tok.mType = Token::Type::NIL;
    tok.mValue = strdup(yytext);
    __toks.push_back(tok);
}

";"|","|"."|","|"<"|">"|"="|"*"|"/"|"+"|"-"|"%"|"++"|"--"|"+="|"-="|"&&"|"|"|"<="|">="|"=="|"{"|"}"|"["|"]"|"("|")" {
    Token tok;
    tok.mValue = strdup(yytext);
    tok.mType = Token::Type::OPERATOR;
    __toks.push_back(tok);
}

"var"|"if"|"for"|"do"|"fun"|"return"|"break"|"continue"|"else" {
    Token tok;
    tok.mValue = strdup(yytext);
    tok.mType = Token::Type::KEYWORD;
    __toks.push_back(tok);
}

"true"|"false" {
    Token tok;
    tok.mValue = strdup(yytext);
    tok.mType = Token::Type::BOOLEAN;
    __toks.push_back(tok);
}

{VARIABLE} {
    Token tok;
    tok.mType = Token::Type::VARIABLE_REF;
    tok.mValue = strdup(yytext);
    __toks.push_back(tok); 
}

[ \t\n]+          /* eat up whitespace */

.  printf( "Unrecognized character: %s\n", yytext );

%%
 
/*** User code section***/
int yywrap(){
    return 1;
}