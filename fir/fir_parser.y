%{
//-- don't change *any* of these: if you do, you'll break the compiler.
#include <algorithm>
#include <memory>
#include <cstring>
#include <cdk/compiler.h>
#include <cdk/types/types.h>
#include "ast/all.h"
#define LINE                         compiler->scanner()->lineno()
#define yylex()                      compiler->scanner()->scan()
#define yyerror(compiler, s)         compiler->scanner()->error(s)
//-- don't change *any* of these --- END!
%}

%parse-param {std::shared_ptr<cdk::compiler> compiler}

%union {
  //--- don't change *any* of these: if you do, you'll break the compiler.
  YYSTYPE() : type(cdk::primitive_type::create(0, cdk::TYPE_VOID)) {}
  ~YYSTYPE() {}
  YYSTYPE(const YYSTYPE &other) { *this = other; }
  YYSTYPE& operator=(const YYSTYPE &other) { type = other.type; return *this; }

  std::shared_ptr<cdk::basic_type> type;        /* expression type */
  //-- don't change *any* of these --- END!

  int                   i;	/* integer value */
  std::string          *s;	/* symbol name or string literal */
  cdk::basic_node      *node;	/* node pointer */
  cdk::sequence_node   *sequence;
  cdk::expression_node *expression; /* expression nodes */
  cdk::lvalue_node     *lvalue;
};

%token <i> tINTEGER
%token <s> tIDENTIFIER tSTRING
%token tFOR tWHILE tIF tWRITE tWRITELN tREAD tBEGIN tEND tRETURN tLEAVE tRESTART
%token tTYPE_STRING tTYPE_INT tTYPE_FLOAT tTYPE_POINTER tTYPE_VOID
%token tPUBLIC tPRIVATE tREQUIRE

%nonassoc tIFX
%nonassoc tELSE tFINALLY

%right '='
%left tGE tLE tEQ tNE '>' '<'
%left '+' '-'
%left '*' '/' '%'
%nonassoc tUNARY

%type <node> stmt
//program
//vardec
%type <sequence> list exprs
//vardecs
//%type<sequence> opt_exprs opt_vardecs
%type <expression> expr
%type <lvalue> lval
//%type <type> data_type
//%type <expr> opt_initializer

%{
//-- The rules below will be included in yyparse, the main parsing function.
%}
%%

//program	: tBEGIN list tEND { compiler->ast(new fir::program_node(LINE, $2)); }
//	    ;

list : stmt	     { $$ = new cdk::sequence_node(LINE, $1); }
	 | list stmt { $$ = new cdk::sequence_node(LINE, $2, $1); }
	 ;

stmt : expr ';'                                   { $$ = new fir::evaluation_node(LINE, $1); }
     //| vardec ';'                                 { $$ = $1; }
 	 | tWRITE exprs ';'                           { $$ = new fir::write_node(LINE, $2, false); }
     | tWRITELN exprs ';'                         { $$ = new fir::write_node(LINE, $2, true); }
     | tREAD lval ';'                             { $$ = new fir::read_node(LINE, $2); }
     | tFOR '(' expr ';' expr ';' expr ')' stmt   { $$ = new fir::for_node(LINE, $3, $5, $7, $9); }
     | tWHILE '(' expr ')' stmt                   { $$ = new fir::while_node(LINE, $3, $5); }
     | tWHILE '(' expr ')' stmt tFINALLY stmt     { $$ = new fir::while_node(LINE, $3, $5, $7); }
     | tIF '(' expr ')' stmt %prec tIFX           { $$ = new fir::if_node(LINE, $3, $5); }
     | tIF '(' expr ')' stmt tELSE stmt           { $$ = new fir::if_else_node(LINE, $3, $5, $7); }
     | tLEAVE ';'                                 { $$ = new fir::leave_node(LINE);  }
     | tLEAVE tINTEGER ';'                        { $$ = new fir::leave_node(LINE, $2);  }
     | tRESTART';'                                { $$ = new fir::restart_node(LINE);  }
     | tRESTART tINTEGER ';'                      { $$ = new fir::restart_node(LINE, $2);  }
     | tRETURN ';'                                { $$ = new fir::return_node(LINE); }
     | '{' list '}'                               { $$ = $2; }
     ;
     //| tRETURN ';'                                { $$ = new fir::return_node(LINE, nullptr); }
     
expr : tINTEGER                { $$ = new cdk::integer_node(LINE, $1); }
	 | tSTRING                 { $$ = new cdk::string_node(LINE, $1); }
     | '-' expr %prec tUNARY   { $$ = new cdk::neg_node(LINE, $2); }
     | expr '+' expr	       { $$ = new cdk::add_node(LINE, $1, $3); }
     | expr '-' expr	       { $$ = new cdk::sub_node(LINE, $1, $3); }
     | expr '*' expr	       { $$ = new cdk::mul_node(LINE, $1, $3); }
     | expr '/' expr	       { $$ = new cdk::div_node(LINE, $1, $3); }
     | expr '%' expr	       { $$ = new cdk::mod_node(LINE, $1, $3); }
     | expr '<' expr	       { $$ = new cdk::lt_node(LINE, $1, $3); }
     | expr '>' expr	       { $$ = new cdk::gt_node(LINE, $1, $3); }
     | expr tGE expr	       { $$ = new cdk::ge_node(LINE, $1, $3); }
     | expr tLE expr           { $$ = new cdk::le_node(LINE, $1, $3); }
     | expr tNE expr	       { $$ = new cdk::ne_node(LINE, $1, $3); }
     | expr tEQ expr	       { $$ = new cdk::eq_node(LINE, $1, $3); }
     | '(' expr ')'            { $$ = $2; }
     | lval                    { $$ = new cdk::rvalue_node(LINE, $1); }  //FIXME
     | lval '=' expr           { $$ = new cdk::assignment_node(LINE, $1, $3); }
     ;

exprs: expr                    { $$ = new cdk::sequence_node(LINE, $1);     }
     | exprs ',' expr          { $$ = new cdk::sequence_node(LINE, $3, $1); }
     ;

//opt_exprs : /* empty */        { $$ = new cdk::sequence_node(LINE); }
//          | exprs              { $$ = $1; }
//          ;

lval : tIDENTIFIER             { $$ = new cdk::variable_node(LINE, $1); }
     ;

//vardec  : tREQUIRE data_type  tIDENTIFIER                         { $$ = new fir::variable_declaration_node(LINE, tPUBLIC,  $2, *$3, nullptr); }
//        | tPUBLIC  data_type  tIDENTIFIER         opt_initializer { $$ = new fir::variable_declaration_node(LINE, tPUBLIC,  $2, *$3, $4); }
//        |          data_type  tIDENTIFIER         opt_initializer { $$ = new fir::variable_declaration_node(LINE, tPRIVATE, $1, *$2, $3); }
//        ;

//vardecs : vardec ';'         { $$ = new cdk::sequence_node(LINE, $1);     }
//        | vardecs vardec ';' { $$ = new cdk::sequence_node(LINE, $2, $1); }
//        ;
             
//opt_vardecs  : /* empty */ { $$ = NULL; }
//             | vardecs     { $$ = $1; }
//             ;

//data_type : tTYPE_STRING                     { $$ = cdk::primitive_type::create(4, cdk::TYPE_STRING);  }
//          | tTYPE_INT                        { $$ = cdk::primitive_type::create(4, cdk::TYPE_INT);     }
//          | tTYPE_FLOAT                      { $$ = cdk::primitive_type::create(8, cdk::TYPE_DOUBLE);  }
//          | tTYPE_POINTER '<' data_type '>'  { $$ = cdk::reference_type::create(4, $3); }
//          ;

//opt_initializer  : /* empty */         { $$ = nullptr; /* must be nullptr, not NIL */ }
//                 | '=' expr      { $$ = $2; }
//                 ;

%%
