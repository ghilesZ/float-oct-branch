(*
  An abstract fixpoint solver based on Constraint Programming

  Author: Antoine Mine
  Copyright 2014
*)

(*
  Uses the parser to parse a file
*)

open Syntax
open Lexing


let string_of_position p =
  Printf.sprintf "%s:%i:%i" p.pos_fname p.pos_lnum (p.pos_cnum - p.pos_bol)

(* open a file and parse it *)
let parse (filename:string) : prog =
  let f = open_in filename in
  let lex = from_channel f in
  try
    lex.lex_curr_p <- { lex.lex_curr_p with pos_fname = filename; };
    Parser.file Lexer.token lex
  with
  | Parser.Error ->
     Printf.eprintf "Parse error (invalid syntax) near %s\n"
                    (string_of_position lex.lex_start_p);
     failwith "Parse error"
  | Failure x ->
     Printf.eprintf "Parse error near %s. %s\n"
                    (string_of_position lex.lex_start_p)
                     x;
     failwith "Parse error"
