(*
  An abstract fixpoint solver based on Constraint Programming

  Author: Antoine Mine
  Copyright 2014
*)

(*
  Main entry point

  The solver functions are actually in the Soup module
*)

open Parameters

(* old algorithm, rational boxes *)
module A1 = Abstract_box.BoxQ
module I1 = Absinterp.Interpreter(A1)
module S1 = Old_soup.Soup(A1)

(* new algorithm, float boxes *)
module A2 = Abstract_box.BoxF
module I2 = Absinterp.PSInterpreter(A2)
module S2 = Soup.Soup(A2)

(* new algorithm, float octagons *)
module A3 = Abstract_apron.OctF
module I3 = Absinterp.PSInterpreter(A3)
module S3 = Soup.Soup(A3)

(* new algorithm, rational boxes *)
module A4 = Abstract_box.BoxQ
module I4 = Absinterp.PSInterpreter(A4)
module S4 = Soup.Soup(A4)


(* do work after arguments have been processed *)
let do_work filename =
  let p = File_parser.parse filename in
  if !legacy_mode then
    let init,goal,body = I1.prog p in
    S1.solve body init goal
  else if !octagon_mode then
    let init,goal,body = I3.prog p in
    S3.solve body init goal
  else if !rational_mode then
    let init,goal,body = I4.prog p in
    S4.solve body init goal
  else
    let init,goal,body = I2.prog p in
    S2.solve body init goal


(* process command-line arguments *)
let parse_args () =
  let files = handle_cmdline (List.tl (Array.to_list Sys.argv)) in
  match files with
  | [f] -> do_work f
  | [] -> failwith "no filename specified"
  | _ -> failwith "too many filenames"


(* do it *)
let _ = parse_args ()
