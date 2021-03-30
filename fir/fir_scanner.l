%option c++ prefix="fir_scanner_" outfile="fir_scanner.cpp"
%option stack noyywrap yylineno 8bit
%{ 
// make relevant includes before including the parser's tab file
#include <string>
#include <cdk/ast/sequence_node.h>
#include <cdk/ast/expression_node.h>
#include <cdk/ast/lvalue_node.h>
#include "fir_parser.tab.h"

// don't change this
#define yyerror LexerError
%}
INT       [[:digit:]]+

%x X_STRING X_TILDE X_COMMENT X_OCT_INT
%%

  /* ====================================================================== */
  /* ====[                      Comentarios                           ]==== */
  /* ====================================================================== */

"!!".*$                         /* match the whole line (it is ignored) */

"(*"                            yy_push_state(X_COMMENT);
<X_COMMENT>"*)"                 yy_pop_state();
<X_COMMENT>.|"\n"               ; /* ignore inside comments */

"--".*                          ; /* ignore comments */

  /* ====================================================================== */
  /* ====[                    Operadores de expressões                ]==== */
  /* ====================================================================== */

[\[\](),?]                      return *yytext;
[-+*/%~<>@?=]                   return *yytext;
">="                            return tGE;
"<="                            return tLE;
"=="                            return tEQ;
"!="                            return tNE;
"&&"                            return tAND;
"||"                            return tOR;
"@"                             return tINPUT;

  /* ====================================================================== */
  /* ====[                     Tipos de dados                         ]==== */
  /* ====[                     Null pointer                           ]==== */
  /* ====[                     Procedures                             ]==== */
  /* ====================================================================== */

"void"                          return tTYPE_VOID;
"int"                           return tTYPE_INT;
"float"                         return tTYPE_FLOAT;
"string"                        return tTYPE_STRING;
[<>]                            return tTYPE_POINTER;
"null"                          return tNULLPTR;

  /* ====================================================================== */
  /* ====[                    Instrução condicional                   ]==== */
  /* ====================================================================== */

"if"                            return tIF; 
"else"                          return tELSE;

  /* ====================================================================== */
  /* ====[                          Símbolos globais                  ]==== */
  /* ====================================================================== */

[*]                             return tPUBLIC;
[?]                             return tREQUIRE;

  /* ====================================================================== */
  /* ====[                       Instrução de iteração                ]==== */
  /* ====[                       Instrução de terminação              ]==== */
  /* ====[                       Instrução de continuação             ]==== */
  /* ====================================================================== */

"begin"                         return tBEGIN; 
"end"                           return tEND;
"return"                        return tRETURN;
"while"                         return tWHILE; 
"finally"                       return tFINALLY;
"leave"                         return tLEAVE;
"restart"                       return tRESTART;


  /* ====================================================================== */
  /* ====[                    Delimitadores e separadores             ]==== */
  /* ====[                    Corpo (de função)                       ]==== */
  /* ====[                    Instrução de retorno                    ]==== */
  /* ====================================================================== */

[#\[\](){};:!,]                 return *yytext;
"return"                        return tRETURN;
"write"                         return tWRITE;
"writeln"                       return tWRITELN;

  /* ====================================================================== */
  /* ====[                           Inteiros                         ]==== */
  /* ====================================================================== */

"0[0-7]"                        yy_push_state(X_OCT_INT);

<X_OCT_INT>[0-7]*               { yylval.i = strtoul(yytext, NULL, 8); yy_pop_state(); return tINTEGER; }
<X_OCT_INT>.|\n                 std::cerr << "WARNING: line " << lineno() << ": bad octal data!" << std::endl; exit(1);

{INT}                           yylval.i = strtoul(yytext, NULL, 10); return tINTEGER;


  /* ====================================================================== */
  /* ====[                           Reais                            ]==== */
  /* ====================================================================== */

([0-9]*\.[0-9]+|[0-9]+\.[0-9]*)([eE][-+]?[0-9]+)? { yylval.d = strtod(yytext, NULL); return tREAL; }


  /* ====================================================================== */
  /* ====[                      Identificadores (nomes)               ]==== */
  /* ====================================================================== */

/* LETTER  [A-Za-z_] */
/* IDENTIFIER   {LETTER}({LETTER}|[[:digit:]])* */

/* {IDENTIFIER}  yylval.s = new std::string(yytext); return tID; */
[A-Za-z][A-Za-z0-9_]*           yylval.s = new std::string(yytext); return tIDENTIFIER;


  /* ====================================================================== */
  /* ====[                      Cadeias de caracteres                 ]==== */
  /* ====================================================================== */

~'                              yy_push_state(X_STRING); yylval.s = new std::string("");
<X_STRING>~'                    yy_pop_state(); return tSTRING;
<X_STRING>~0                    yy_pop_state(); return tSTRING;
<X_STRING>~~~'                  *yylval.s += yytext + 1;
<X_STRING>.                     *yylval.s += yytext;

<X_STRING>~~                    yy_push_state(X_TILDE);
/* <X_STRING>.                     strlit << *yytext; */

<X_TILDE>n                      strlit << '~n'; yy_pop_state();
<X_TILDE>r                      strlit << '~r'; yy_pop_state();
<X_TILDE>t                      strlit << '~t'; yy_pop_state();
<X_TILDE>~                      strlit << '~~'; yy_pop_state();

<X_TILDE>[[:xdigit:]]{1,2}      {
                                strlit << (char)(unsigned char)strtoul(yytext, NULL, 16);
                                yy_pop_state();
                                }

<X_TILDE>.                      strlit << *yytext; yy_pop_state();

[0-9]+                          yylval.i = strtol(yytext, nullptr, 10); return tINTEGER;

[-()<>=+*/%;{}.]                return *yytext;

[ \t\n]+                        ; /* ignore whitespace */

.                               yyerror("Unknown character");

  /* ====================================================================== */
  /* ====[                    Outros caracteres                       ]==== */
  /* ====================================================================== */

[[:space:]]                     /* ignore remaining white space inside the program */
.                               yyerror(yytext);    /* all other chars: error! */

%%