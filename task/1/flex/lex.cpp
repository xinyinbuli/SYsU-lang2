#include "lex.hpp"
#include <iostream>

void
print_token();

namespace lex {

static const char* kTokenNames[] = {
    "identifier",    "numeric_constant", "string_literal", "int",
    "void",          "const",            "return",         "if",
    "else",          "while",            "break",          "continue",
    "l_brace",       "r_brace",          "l_square",       "r_square",
    "l_paren",       "r_paren",          "semi",           "lessequal",
    "greaterequal",  "exclaimequal",     "equalequal",     "equal",
    "exclaim",       "greater",          "less",           "pipepipe",
    "ampamp",        "pipe",             "amp",            "plus",
    "minus",         "star",             "slash",          "percent",
    "comma"
};


const char*
id2str(Id id)
{
  static char sCharBuf[2] = { 0, 0 };
  if (id == Id::YYEOF) {
    return "eof";
  }
  else if (id < Id::IDENTIFIER) {
    sCharBuf[0] = char(id);
    return sCharBuf;
  }
  return kTokenNames[int(id) - int(Id::IDENTIFIER)];
}

G g;
int brows=0;
int
come(int tokenId, const char* yytext, int yyleng, int yylineno)
{
  g.mId = Id(tokenId);
  g.mText = { yytext, std::size_t(yyleng) };
  yylineno-=brows;
  g.mStartOfLine = g.mLine!=yylineno;
  if(g.mStartOfLine) {
    g.mLine = yylineno;
  }
  print_token();
  g.mColumn+=yyleng;
  g.mLeadingSpace = false;

  return tokenId;
}
void getfilename(const char* yytext, int yylineno){
  int i=0;
  std::string s;
  while(yytext[i]<'0'||yytext[i]>'9')i++;
  brows=0;
  while(yytext[i]>='0'&&yytext[i]<='9'){
    brows*=10;
    brows+=yytext[i++]-'0';
  }
  brows=yylineno-brows+1;
  while(yytext[i]!='\"'){
    i++;
  }
  i++;
  while(yytext[i]!='\"'){
    s+=yytext[i++]; 
  } 
  g.mFile=s;
}
void spacecontrol(const char* yytext){
    switch (*yytext)
    {
    case ' ':
      g.mLeadingSpace = true;
      g.mColumn+=1;
      break;
    case '\t':
      g.mLeadingSpace = true;
      g.mColumn=(g.mColumn-1)/8*8+9;
      break;
    case '\n':
      g.mColumn=1;
      break;
    default:
      break;
    }
}
} // namespace lex
