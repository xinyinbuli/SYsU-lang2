%{
#include "lex.hpp"
/* 所有代码全部抽离出来，放到 lex.hpp 和 lex.cpp 里 */

using namespace lex;

#define COME(id) return come(id, yytext, yyleng, yylineno)
#define GFN() getfilename(yytext,yylineno)
#define SC() spacecontrol(yytext)
%}

%option 8bit warn noyywrap yylineno

D     [0-9]
L     [a-zA-Z_]
F     [0-9a-f]
IS    ((u|U)|(u|U)?(l|L|ll|LL)|(l|L|ll|LL)(u|U))

%%

"int"       {  COME(INT); }
"void"      {  COME(VOID); }
"const"     {  COME(CONST); }
"return"    {  COME(RETURN); }
"if"        {  COME(IF); }
"else"      {  COME(ELSE); }
"while"     {  COME(WHILE); }
"break"     {  COME(BREAK); }
"continue"     {  COME(CONTINUE); }

"("         {  COME(L_PAREN); }
")"         {  COME(R_PAREN); }
"["         {  COME(L_SQUARE); }
"]"         {  COME(R_SQUARE); }
"{"         {  COME(L_BRACE); }
"}"         {  COME(R_BRACE); }

"+"         {  COME(PLUS); }
"-"         {  COME(MINUS); }
"*"         {  COME(STAR); }
"/"         {  COME(SLASH); }
"%"         {  COME(PERCENT); }


";"         {  COME(SEMI); }
","         {  COME(COMMA); }

"<="        {  COME(LESSEQUAL); }
">="        {  COME(GREATEREQUAL); }
"!="        {  COME(EXCLAIMEQUAL); }
"=="        {  COME(EQUALEQUAL); }
"="         {  COME(EQUAL); }
"!"         {  COME(EXCLAIM); }
">"         {  COME(GREATER); }
"<"         {  COME(LESS); }
"||"        {  COME(PIPEPIPE); }
"&&"        {  COME(AMPAMP); }
"|"        {  COME(PIPE); }
"&"        {  COME(AMP); }


{L}({L}|{D})*         {  COME(IDENTIFIER); }

L?\"(\\.|[^\\"\n])*\" {  COME(STRING_LITERAL); }

0[0-7]*{IS}?          {  COME(CONSTANT); }
[1-9]{D}*{IS}?        {  COME(CONSTANT); }
0x{F}*{IS}?        {  COME(CONSTANT); }

^#[^\n]*              { GFN();return ~YYEOF; } /* 预处理信息处理，可以从预处理信息中获得文件名以及行号 */

[ \t\v\n\f]           { SC();return ~YYEOF; } /* 需要处理行号和列号信息 */

<<EOF>>     {  COME(YYEOF); }

%%

/* about symbols avaliable (yytext, yyleng etc.) in the context of Flex:
 * https://ftp.gnu.org/old-gnu/Manuals/flex-2.5.4/html_node/flex_14.html
 * https://ftp.gnu.org/old-gnu/Manuals/flex-2.5.4/html_node/flex_15.html
*/
