%option noyywrap
%{
#include <iostream>
using namespace std;
#define YY_DECL extern "C" int yylex()
#include "AST.h"
#include "parser.tab.h"
char temp[100];
%}

COMMENT  "//".*
COMMENTS  "/*"(.|\n)*"*/"

CHAR ([\40-\176]{-}['"\\])|\\n|\\t|\\\\|\\\"|\\\'

%%


{COMMENT}   {
                
            }

{COMMENTS}  {
                
            }
[ \t\n]                 ;

"class"                 { return CLASS; }

"Program"               { return PROGRAM; }

"void"                  { return VOID; }

"true"                  { yylval.bval = true; return BOOLEAN; }

"false"                 { yylval.bval = false; return BOOLEAN; }

int                     { return INT_DECLARATION; }

boolean                 { return BOOLEAN_DECLARATION; }

"if"                    { return IF; }

"for"                   { return FOR; }

"else"                  { return ELSE; }

"return"                { return RETURN; }

"break"                 { return BREAK; }

"continue"              { return CONTINUE; }

"callout"               { return CALLOUT; }

[a-zA-Z_][a-zA-Z0-9_]*  { yylval.sval = strdup(yytext); return ID; }

"+"                     {return ADD; }
"-"                     {return SUB; }
"*"                     {return MUL; }
"/"                     {return DIV; }
"%"                     {return MOD; }
                                
"<"                     {return SMALLER; }
">"                     {return GREATER; }
"<="                    {return ESMALLER; }
">="                    {return EGREATER; }
"=="                    {return EQUALEQUAL; }
"!="                    {return NOTEQUAL; }

"="                     {return EQUAL; }
"+="					{return PLUS_EQUAL; }
"-="               		{return MINUS_EQUAL; }

"&&"                    {return AND; }
"||"                    {return OR; }

"("|")"|"{"|"}"|"["|"]"|","|"!"|";"     { return yytext[0]; }

\"{CHAR}*\"             {  for(int i = 0; i < 100; i++) temp[i] = '\0'; strncpy(temp, yytext + 1, strlen(yytext)-2); yylval.sval = temp; return STRING; }

\'{CHAR}\'              { yylval.cval = yytext[1]; return CHARACTER; }

0x[0-9a-fA-F]+          { yylval.ival = atoi(yytext); return INT; }

[0-9]+                  { yylval.ival = atoi(yytext); return INT; }

.                       {}
%%
