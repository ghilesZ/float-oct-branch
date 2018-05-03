(*
  An abstract fixpoint solver based on Constraint Programming

  Author: Antoine Mine
  Copyright 2015
*)

(*
  Solver parameters
*)



(* sanity check, but only for the result *)
let do_validate_final = ref false

(* sanity check at each step (slow!) *)
let do_validate_step = ref false

(* log actions *)
let verbose_log = ref false

(* timeout *)
let max_iterate = ref 50000000

(* epsilon *)
let epsilon_size = ref 0.05
let epsilon_cover = ref 0.1

(* graphic output *)
let svg_width, svg_height = ref 880, ref 880  (* size *)
let svg_result = ref true                     (* dumps only the result *)
let svg_steps = ref false                     (* dumps intermediate steps *)
let svg_animation = ref false
let svg_x, svg_y = ref "", ref ""             (* variables *)
let svg_prefix = ref "out/output_"

(* parameters specific to the new algorithm *)
(* **************************************** *)


(* dump the result in text form *)
let print_dump = ref false

(* keep abstract elements tighetened at all time *)
let aggressive_tightening = ref true

(* invariant refinement algorithm *)
let max_refinement_steps = ref 4
let resplit_threshold = ref 12
let resplit_steps = ref 2
let fringe_size = ref 1
let epsilon_falloff = ref 0.5

(* refinement steps after failure *)
let max_search_steps = ref 8

(* failure handing *)
let resplit_failed_threshold = ref 0
let resplit_failed_steps = ref 2

(* show the partition in the graphic output *)
let svg_show_partition = ref false



(* mode parameters *)
(* *************** *)


let legacy_mode = ref false
let octagon_mode = ref false
let rational_mode = ref false


(* parameter handling *)
(* ****************** *)

type param =
  | Param_int of int ref
  | Param_float of float ref
  | Param_string of string ref
  | Param_bool of bool ref
  | Param_set of bool ref
  | Param_clear of bool ref


let params =

  ["check", Param_set do_validate_step;
   "verbose", Param_set verbose_log;
   "max-iterate", Param_int max_iterate;
   "epsilon-size", Param_float epsilon_size;
   "epsilon-cover", Param_float epsilon_cover;
   "svg-width", Param_int svg_width;
   "svg-height", Param_int svg_height;
   "svg-result", Param_bool svg_result;
   "svg-steps", Param_bool svg_steps;
   "svg-animation", Param_bool svg_animation;
   "svg-x", Param_string svg_x;
   "svg-y", Param_string svg_y;
   "svg-prefix", Param_string svg_prefix;
   "svg-time", Param_float Svg.total_time;

   "dump", Param_set print_dump;
   "aggressive-tighetening", Param_bool aggressive_tightening;
   "max-refinement-steps", Param_int max_refinement_steps;
   "max-failure-search-steps", Param_int max_search_steps;
   "resplit-threshold", Param_int resplit_threshold;
   "resplit-steps", Param_int resplit_steps;
   "peel-size", Param_int fringe_size;
   "epsilon-falloff", Param_float epsilon_falloff;
   "resplit-failed-threshold", Param_int resplit_failed_threshold;
   "resplit-failed-steps", Param_int resplit_failed_steps;
   "svg-show-partition", Param_set svg_show_partition;

   "legacy", Param_set legacy_mode;
   "octagon", Param_set octagon_mode;
   "rational", Param_set rational_mode;
 ]


(* list of all options and default values *)
let print_help () =
  Printf.printf "recognized options:\n";
  List.iter
    (fun (opt,v) ->
      Printf.printf "  --%s " opt;
      match v with
      | Param_int r -> Printf.printf "int (%i)\n" !r
      | Param_float r -> Printf.printf "float (%f)\n" !r
      | Param_bool r -> Printf.printf "bool (%B)\n" !r
      | Param_string r -> Printf.printf "string (%s)\n" !r
      | Param_set r -> Printf.printf "(%B)\n" !r
      | Param_clear r -> Printf.printf "(%B)\n" !r
    ) params


(* handle options and returns the list of non-options *)
let handle_cmdline (cmd:string list) : string list =
  if List.exists (fun s -> s="-help" || s="--help") cmd
  then (print_help(); exit 0);

  let rec doit cmd acc = match cmd with
  | [] -> List.rev acc
  | a::rest ->
      if a = "" then doit rest acc
      else if a.[0] = '-' then (
        let l = String.length a in
        let x =
          if l > 1 && a.[1] = '-' then String.sub a 2 (l-2)
          else String.sub a 1 (l-1)
        in
        try match List.assoc x params, rest with
        | Param_int r, v::rest -> r := int_of_string v; doit rest acc
        | Param_float r, v::rest -> r := float_of_string v; doit rest acc
        | Param_bool r, "true"::rest -> r := true; doit rest acc
        | Param_bool r, "false"::rest -> r := false; doit rest acc
        | Param_string r, v::rest ->  r := v; doit rest acc
        | Param_set r, rest -> r := true; doit rest acc
        | Param_clear r, rest -> r := false; doit rest acc
        | _ -> failwith ""
        with _ ->
          print_help();
          Printf.printf "\nunknown option or wrong argument type for %s\n" x;
          exit 1
       )
      else doit rest (a::acc)
  in
  doit cmd []
