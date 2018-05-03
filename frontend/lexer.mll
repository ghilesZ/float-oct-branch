(*
  An abstract fixpoint solver based on Constraint Programming
  
  Author: Antoine Mine
  Copyright 2014
*)

(*
  Ocamllex lexer for our simple language
*)

{
 open Lexing
 open Syntax
 open Parser


(* keyword table *)
let kwd_table = Hashtbl.create 10
let _ = 
  List.iter (fun (a,b) -> Hashtbl.add kwd_table a b)
    [
     "if",     TOK_IF;
     "else",   TOK_ELSE;
     "abs",    TOK_ABS;
     "sqrt",   TOK_SQRT;
     "init",   TOK_INIT;
     "body",   TOK_BODY;
     "goal",   TOK_GOAL;
   ]

(* (exact) parsing of decimal constants constants *)
let parse_const c =
  let rec div10 x n =
    if n <= 0 then x else div10 (Q.div x (Q.of_int 10)) (n-1)
  in
  try
    let p = String.index c '.' in
    let p' = String.length c - p - 1 in
    let x = (String.sub c 0 p)^(String.sub c (p+1) p') in
    div10 (Q.of_string x) p'
  with Not_found ->
    Q.of_string c
}
      

(* character classes *)
let space = [' ' '\t' '\r']+
let newline = "\n" | "\r" | "\r\n"
let digit = ['0'-'9']
let const = "-"? (digit+ | "." digit+ | digit+ "." digit*)

rule token = parse

(* identifier or reserved keyword *)
| ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']* '%'? as id
{ try Hashtbl.find kwd_table id with Not_found -> TOK_id id }


(* symbols *)
| "("    { TOK_LPAREN }
| ")"    { TOK_RPAREN }
| "{"    { TOK_LBRACE }
| "}"    { TOK_RBRACE }
| "["    { TOK_LBRACKET }
| "]"    { TOK_RBRACKET }
| ","    { TOK_COMMA }
| ";"    { TOK_SEMICOLON }
| "+"    { TOK_PLUS }
| "-"    { TOK_MINUS }
| "*"    { TOK_MULTIPLY }
| "/"    { TOK_DIVIDE }
| "<"    { TOK_LESS }
| ">"    { TOK_GREATER }
| "<="   { TOK_LESS_EQUAL }
| ">="   { TOK_GREATER_EQUAL }
| "=="   { TOK_EQUAL_EQUAL }
| "!="   { TOK_NOT_EQUAL }
| "<%"    { TOK_LESS_INT }
| ">%"    { TOK_GREATER_INT }
| "<=%"   { TOK_LESS_EQUAL_INT }
| ">=%"   { TOK_GREATER_EQUAL_INT }
| "==%"   { TOK_EQUAL_EQUAL_INT }
| "!=%"   { TOK_NOT_EQUAL_INT }
| "="    { TOK_ASSIGN }
| "&&"   { TOK_AND }
| "||"   { TOK_OR }
| "!"    { TOK_NOT }
| "?"    { TOK_RANDOM }
    
(* literals *)
| const as c { TOK_const (parse_const c) }

(* spaces, comments *)
| "/*" { comment lexbuf; token lexbuf }
| "//" [^ '\n' '\r']* { token lexbuf }
| newline { new_line lexbuf; token lexbuf }
| space { token lexbuf }

(* end of file *)
| eof { TOK_EOF }


(* nested comments (handled recursively)  *)
and comment = parse
| "*/" { () }
| [^ '\n' '\r'] { comment lexbuf }
| newline { new_line lexbuf; comment lexbuf }
