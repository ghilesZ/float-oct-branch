(*
  An abstract fixpoint solver based on Constraint Programming
  
  Author: Antoine Mine
  Copyright 2014
*)

(*
  Menhir parser for our simple language.

  One expected shift/reduce conflict: classic if/then/else
*)

%{
open Syntax
%}


/* tokens */
%token TOK_LBRACE
%token TOK_RBRACE
%token TOK_LBRACKET
%token TOK_RBRACKET
%token TOK_LPAREN
%token TOK_RPAREN
%token TOK_COMMA
%token TOK_SEMICOLON
%token TOK_PLUS
%token TOK_MINUS
%token TOK_MULTIPLY
%token TOK_DIVIDE
%token TOK_LESS
%token TOK_GREATER
%token TOK_LESS_EQUAL
%token TOK_GREATER_EQUAL
%token TOK_EQUAL_EQUAL
%token TOK_NOT_EQUAL
%token TOK_LESS_INT
%token TOK_GREATER_INT
%token TOK_LESS_EQUAL_INT
%token TOK_GREATER_EQUAL_INT
%token TOK_EQUAL_EQUAL_INT
%token TOK_NOT_EQUAL_INT
%token TOK_ASSIGN
%token TOK_AND
%token TOK_OR
%token TOK_NOT
%token TOK_RANDOM 

%token TOK_ABS
%token TOK_SQRT
%token TOK_IF
%token TOK_ELSE
%token TOK_INIT
%token TOK_GOAL
%token TOK_BODY

%token <string> TOK_id
%token <Q.t> TOK_const

%token TOK_EOF

/* priorities */
%left TOK_OR
%left TOK_AND
%nonassoc TOK_NOT
%left TOK_PLUS TOK_MINUS
%left TOK_MULTIPLY TOK_DIVIDE
%nonassoc unary_minus
%nonassoc TOK_ABS TOK_SQRT

/* entry point */
%start<Syntax.prog> file


%%

file: 
  TOK_INIT s1=stat
  TOK_BODY s2=stat
  TOK_GOAL s3=stat
  TOK_EOF
  { { init=s1; body=s2; goal=s3; } }


stat:
| i=TOK_id TOK_ASSIGN e=expr TOK_SEMICOLON 
    { Assign (i, e) } 
| TOK_IF TOK_LPAREN e=bexpr TOK_RPAREN s1=stat 
    { If (e, s1, Block []) }
| TOK_IF TOK_LPAREN e=bexpr TOK_RPAREN s1=stat TOK_ELSE s2=stat 
    { If (e, s1, s2) }
| TOK_LBRACE b=list(stat) TOK_RBRACE 
    { Block b }


expr:
| i=TOK_id                            { Var i }
| e1=expr o=binary_op e2=expr         { Binary (o,e1,e2) }
| TOK_MINUS e=expr %prec unary_minus  { Unary (NEG,e) }
| TOK_ABS e=expr                      { Unary (ABS,e) }
| TOK_SQRT e=expr                     { Unary (SQRT,e) }
| c=TOK_const                         { Cst (c,c) }
| TOK_LPAREN e=expr TOK_RPAREN        { e }
| TOK_LBRACKET c1=TOK_const TOK_COMMA c2=TOK_const TOK_RBRACKET 
    { Cst (c1,c2) }

%inline binary_op:
| TOK_PLUS     { ADD } 
| TOK_MINUS    { SUB }
| TOK_MULTIPLY { MUL }
| TOK_DIVIDE   { DIV }

bexpr:
| e1=expr o=cmp_op e2=expr  { Cmp (o,e1,e2) }
| e1=bexpr TOK_AND e2=bexpr { And (e1,e2) }
| e1=bexpr TOK_OR e2=bexpr  { Or (e1,e2) }
| TOK_NOT e=bexpr           { Not e }
| TOK_LPAREN e=bexpr TOK_RPAREN  { e }
| TOK_RANDOM { Rand }

%inline cmp_op:
| TOK_LESS              { LT }
| TOK_GREATER           { GT }
| TOK_LESS_EQUAL        { LEQ }
| TOK_GREATER_EQUAL     { GEQ }
| TOK_EQUAL_EQUAL       { EQ }
| TOK_NOT_EQUAL         { NEQ }
| TOK_LESS_INT          { LT_INT }
| TOK_GREATER_INT       { GT_INT }
| TOK_LESS_EQUAL_INT    { LEQ_INT }
| TOK_GREATER_EQUAL_INT { GEQ_INT }
| TOK_EQUAL_EQUAL_INT   { EQ_INT }
| TOK_NOT_EQUAL_INT     { NEQ_INT }

