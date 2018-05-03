
(* The type of tokens. *)

type token = 
  | TOK_id of (string)
  | TOK_const of (Q.t)
  | TOK_SQRT
  | TOK_SEMICOLON
  | TOK_RPAREN
  | TOK_RBRACKET
  | TOK_RBRACE
  | TOK_RANDOM
  | TOK_PLUS
  | TOK_OR
  | TOK_NOT_EQUAL_INT
  | TOK_NOT_EQUAL
  | TOK_NOT
  | TOK_MULTIPLY
  | TOK_MINUS
  | TOK_LPAREN
  | TOK_LESS_INT
  | TOK_LESS_EQUAL_INT
  | TOK_LESS_EQUAL
  | TOK_LESS
  | TOK_LBRACKET
  | TOK_LBRACE
  | TOK_INIT
  | TOK_IF
  | TOK_GREATER_INT
  | TOK_GREATER_EQUAL_INT
  | TOK_GREATER_EQUAL
  | TOK_GREATER
  | TOK_GOAL
  | TOK_EQUAL_EQUAL_INT
  | TOK_EQUAL_EQUAL
  | TOK_EOF
  | TOK_ELSE
  | TOK_DIVIDE
  | TOK_COMMA
  | TOK_BODY
  | TOK_ASSIGN
  | TOK_AND
  | TOK_ABS

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val file: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Syntax.prog)
