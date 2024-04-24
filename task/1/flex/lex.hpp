#pragma once

#include <string>
#include <string_view>
#include <cstring>

namespace lex {

enum Id
{
  YYEMPTY = -2,
  YYEOF = 0,     /* "end of file"  */
  YYerror = 256, /* error  */
  YYUNDEF = 257, /* "invalid token"  */
  IDENTIFIER,
  CONSTANT,
  STRING_LITERAL,
  INT,
  VOID,
  CONST,
  RETURN,
  IF,
  ELSE,
  WHILE,
  BREAK,
  CONTINUE,
  L_BRACE,
  R_BRACE,
  L_SQUARE,
  R_SQUARE,
  L_PAREN,
  R_PAREN,
  SEMI,
  LESSEQUAL,
  GREATEREQUAL,
  EXCLAIMEQUAL,
  EQUALEQUAL,
  EQUAL,
  EXCLAIM,
  GREATER,
  LESS,
  PIPEPIPE,
  AMPAMP,
  PIPE,
  AMP,
  PLUS,
  MINUS,
  STAR,
  SLASH,
  PERCENT,
  COMMA
};

const char*
id2str(Id id);

struct G
{
  Id mId{ YYEOF };              // 词号
  std::string_view mText;       // 对应文本
  std::string mFile;            // 文件路径
  int mLine{ 0 }, mColumn{ 1 }; // 行号、列号
  bool mStartOfLine{ true };    // 是否是行首
  bool mLeadingSpace{ false };  // 是否有前导空格
};

extern G g;

int
come(int tokenId, const char* yytext, int yyleng, int yylineno);
void
getfilename(const char* yytext, int yyleng);
void 
spacecontrol(const char* yytext);

} // namespace lex
