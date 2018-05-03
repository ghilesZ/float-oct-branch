(*
  An abstract fixpoint solver based on Constraint Programming
  
  Author: Antoine Mine
  Copyright 2014
*)

(*
  Syntax tree for program fragments in our simple language

  Note: program fragments are loop-free
 *)


(* variables are identified by a string *)
type var = string

(* constants are intervals (model a non-deterministic choice *)
type i = Q.t * Q.t



(* unary arithmetic operators *)
type unop = NEG | ABS | SQRT

(* binary arithmetic operators *)
type binop = ADD | SUB | MUL | DIV

(* arithmetic comparison operators *)
type cmpop =
  | EQ | LEQ | GEQ | NEQ | GT | LT                          (* real *)
  | EQ_INT | LEQ_INT | GEQ_INT | NEQ_INT | GT_INT | LT_INT  (* integer *)


(* numeric expressions *)
type expr =
  | Unary of unop * expr
  | Binary of binop * expr * expr
  | Var of var
  | Cst of i


(* boolean expressions *)
type bexpr =
  | Cmp of cmpop * expr * expr
  | And of bexpr * bexpr
  | Or of bexpr * bexpr
  | Not of bexpr
  | Rand


(* statements *)
type stat =
  | Assign of var * expr
  | If of bexpr * stat * stat  (* condition, then branch, else branch *)
  | Block of stat list         (* can be [] to model nop *)


(* program *)
type prog = { init: stat; body: stat; goal: stat; }
      
