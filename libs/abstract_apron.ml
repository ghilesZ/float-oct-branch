(*
  An abstract fixpoint solver based on Constraint Programming

  Author: Antoine Mine
  Copyright 2014
*)

(*
  Bridge from Apron domains to abstract_sig interface.
*)
open Bot
open Bound_sig
open Itv_sig
open Abstract_sig
open Syntax
open Apron


module type MANAGER = sig
  type lib
  val manager: lib Manager.t
end



(*******************)
(* GENERIC FUNCTOR *)
(*******************)


(* NOTE: currently, we assume a float-based abstract domain,
   such as floating-point octagons
 *)


module Domain(Man:MANAGER) = (struct


  let man = Man.manager


  let poly_man = Polka.manager_alloc_strict ()


  module A = Abstract1


  (************************************************************************)
  (* TYPES *)
  (************************************************************************)

  module I = Itv.ItvF
  module B = I.B
  type bound = B.t
  type i = I.t

  type t = Man.lib A.t


  (* or now, we only split along variables, although relation domains could
     in theory support split along other directions
  *)
  type dir = var



  (*************)
  (* UTILITIES *)
  (*************)

  external mpz_set: Mpz.t -> Z.t -> unit = "ml_z_mlgmpidl_set_mpz"
  external of_mpz: Mpz.t -> Z.t = "ml_z_mlgmpidl_of_mpz"
  let to_mpz (x:Z.t) : Mpz.t = let z = Mpz.init () in mpz_set z x; z
  let to_mpq (x:Q.t) : Mpq.t = Mpq.of_mpz2 (to_mpz x.Q.num) (to_mpz x.Q.den)


  let scalar_to_float (f:Scalar.t) : float =
    match f with
    | Scalar.Float f -> f
    | Scalar.Mpqf m -> Mpqf.to_float m
    | Scalar.Mpfrf f -> Mpfrf.to_float f

  let interval_to_float (i:Interval.t) : float * float =
    scalar_to_float i.Interval.inf, scalar_to_float i.Interval.sup

  let apron_to_box (a:t) : i array =
    assert(not (A.is_bottom man a));
    let b = A.to_box man a in
    Array.map interval_to_float b.A.interval_array

  let rec expr_to_apron env (e:expr) : Texpr1.expr =
    match e with
    | Var v ->
        let var = Var.of_string v in
        if not (Environment.mem_var env var)
        then failwith ("variable not found: "^v);
        Texpr1.Var var
    | Cst (l,h) ->
        Texpr1.Cst (Coeff.i_of_mpq (to_mpq l) (to_mpq h))
    | Unary (o,e1) ->
        let r = match o with
        | NEG -> Texpr1.Neg
        | ABS -> failwith "abs unsupported in abstract_apron"
        | SQRT -> Texpr1.Sqrt
        in
        let e1 = expr_to_apron env e1 in
        Texpr1.Unop (r, e1, Texpr1.Real, Texpr1.Near)
    | Binary (o,e1,e2) ->
        let r = match o with
        | ADD -> Texpr1.Add
        | SUB -> Texpr1.Sub
        | DIV -> Texpr1.Div
        | MUL -> Texpr1.Mul
        in
        let e1 = expr_to_apron env e1
        and e2 = expr_to_apron env e2 in
        Texpr1.Binop (r, e1, e2, Texpr1.Real, Texpr1.Near)


  let bound_var a v (l,h) =
    let env = Abstract1.env a in
    let v = Var.of_string v in
    let ar = Lincons1.array_make env 2 in
    if l <> neg_infinity then (
      let c1 = Lincons1.make( Linexpr1.make env) Lincons1.SUPEQ in
      Lincons1.set_list c1 [Coeff.s_of_int 1, v] (Some (Coeff.s_of_float (-. l)));
      Lincons1.array_set ar 0 c1
     );
    if h <> infinity then (
      let c2 = Lincons1.make( Linexpr1.make env) Lincons1.SUPEQ in
      Lincons1.set_list c2 [Coeff.s_of_int (-1), v] (Some (Coeff.s_of_float h));
      Lincons1.array_set ar 1 c2
     );
    A.meet_lincons_array man a ar



  (************************************************************************)
  (* PRINTING *)
  (************************************************************************)


  let bprint buf a =
    let fmt = Format.formatter_of_buffer buf in
    A.print fmt a;
    Format.pp_print_flush fmt ()

  let to_string a =
    let b = Buffer.create 16 in
    bprint b a;
    Buffer.contents b

  let sprint () a = to_string a
  let output chan a = output_string chan (to_string a)
  let pp_print fmt a = A.print fmt a


  (* NOTE: this is quite inefficient;
     it converts through polyhedra and generator sets;
     ignores non-vertex generators
   *)
  let to_polygon a v1 v2 =
    (* projects on v1,v2 *)
    let env = Environment.make [||] [|Var.of_string v1; Var.of_string v2|] in
    let a = A.change_environment man a env false in
    (* converts to polyhedron *)
    let l = A.to_lincons_array man a in
    let p = A.of_lincons_array poly_man a.A.env l in
    (* gets and converts generators *)
    let g = A.to_generator_array poly_man p in
    let to_float = function
      | Coeff.Scalar s -> scalar_to_float s
      | _ -> failwith "Apron coeff type not supported"
    in
    let g =
      Array.fold_left
        (fun acc g ->
          if g.Generator0.typ <> Generator0.VERTEX then acc else
          let l = g.Generator0.linexpr0 in
          let x,y = Linexpr0.get_coeff l 0, Linexpr0.get_coeff l 1 in
          (to_float x, to_float y)::acc
        ) [] g.Generator1.generator0_array
    in
    if List.length g < 2 then [] else
    (* find leftmost *)
    let (lx,ly) as lg =
      List.fold_left
        (fun (lx,ly) (x,y) -> if x < lx || x = lx && y < ly then (x,y) else (lx,ly))
        (List.hd g) (List.tl g)
    in
    (* order generators clockwise *)
    let slope (x,y) = (y -. ly) /. (x -. lx) in
    List.sort
      (fun ((x0,y0) as g0) ((x1,y1) as g1) ->
        if g0 = g1 then 0 else
        if g0 = lg then -1 else
        if g1 = lg then 1 else
        if x0 = lx then -1 else
        if x1 = lx then 1 else
        if slope g0 < slope g1 then 1 else -1
      ) g



  (************************************************************************)
  (* SET-THEORETIC *)
  (************************************************************************)


  let or_bot a =
    if A.is_bottom man a then Bot else Nb a

  let join a b = A.join man a b

  let meet a b = or_bot (A.meet man a b)

  let join_bot a b = match a,b with
  | Bot,x | x,Bot -> x
  | Nb x, Nb y -> or_bot (A.join man x y)

  let meet_bot a b = match a,b with
  | Bot,_ -> a | _,Bot -> b
  | Nb x, Nb y -> or_bot (A.meet man x y)


  let equal a b = A.is_eq man a b

  let subseteq a b = A.is_leq man a b

  let intersect a b =
    not (A.is_bottom man (A.meet man a b))

  (* we test whether the enclosing box is degenerate *)
  let degenerate a =
    (A.is_bottom man a) ||
    (Array.fold_left (fun r (l,h) -> r || l = h) false (apron_to_box a))

  (* we test whether the enclosing box is degenerate *)
  let intersect_nondegenerate a b =
    degenerate (A.meet man a b)

  let dimension a =
    Environment.size a.A.env

  (* we return the number of non-degenerate dimensinos of the enclosing box *)
  let nondegenerate_dimension a =
    if A.is_bottom man a then 0 else
    Array.fold_left
      (fun r (l,h) -> r + (if l=h then 0 else 1)) 0 (apron_to_box a)

  (* we use the volume of the enclosing box *)
  let volume a =
    if A.is_bottom man a then 0. else
    Array.fold_left (fun vol (l,h) -> vol *. (h -. l)) 1. (apron_to_box a)

  let exact_volume a =
    failwith "exact_volume not implemented"

  let volume_ext a =
    if A.is_bottom man a then 0. else
    Array.fold_left
      (fun vol (l,h) -> if l = h then vol else vol *. (h -. l))
      1. (apron_to_box a)

  let overlap a b =
    volume (A.meet man a b)

  let max_range a =
    let b = apron_to_box a in
    let var,range = ref 0, ref (-. 1.) in
    for i=0 to Array.length b - 1 do
      let r = (snd b.(i)) -. (fst b.(i)) in
      if r > !range then (range := r; var := i)
    done;
    Var.to_string (Environment.var_of_dim a.A.env !var),
    b.(!var)


  let split a v b =
    let ar = Lincons1.array_make a.A.env 1 in
    let c1 = Lincons1.make( Linexpr1.make a.A.env) Lincons1.SUPEQ in
    Lincons1.set_list c1 [Coeff.s_of_int (-1), (Var.of_string v)] (Some (Coeff.s_of_float b));
    Lincons1.array_set ar 0 c1;
    let a1 = A.meet_lincons_array man a ar in
    let c2 = Lincons1.make( Linexpr1.make a.A.env) Lincons1.SUPEQ in
    Lincons1.set_list c2 [Coeff.s_of_int 1, (Var.of_string v)] (Some (Coeff.s_of_float (-.b)));
    Lincons1.array_set ar 0 c2;
    let a2 = A.meet_lincons_array man a ar in
    or_bot a1, or_bot a2


  let size a =
    let l,h = snd (max_range a) in
    h -. l



  (************************************************************************)
  (* ABSTRACT OPERATIONS *)
  (************************************************************************)


  let init =
    A.top man (Environment.make [||] [||])

  let add_var a v i =
    let env = Environment.add a.A.env [||] [|Var.of_string v|] in
    let a = A.change_environment man a env false in
    bound_var a v i

  let remove_var a v =
    let env = Environment.remove a.A.env [|Var.of_string v|] in
    let a = A.change_environment man a env false in
    a

  let get_var_range a v =
    interval_to_float (A.bound_variable man a (Var.of_string v))

  let get_variables a =
    let _,vars = Environment.vars a.A.env in
    let vars = Array.to_list vars in
    List.map (fun v -> Var.to_string v) vars

  let set_var_range a v i =
    let var = Var.of_string v in
    let a =
      if Environment.mem_var a.A.env var then
        A.forget_array man a [|var|] false
      else
        let env = Environment.add a.A.env [||] [|var|] in
        A.change_environment man a env false
    in
    bound_var a v i


  let unify_meet a b =
    (* construct environment meet *)
    let env = a.A.env in
    let _,vars = Environment.vars env in
    let env =
      Array.fold_left
        (fun env var ->
          if Environment.mem_var b.A.env var then env
          else Environment.remove env [|var|]
        ) env vars
    in
    (* apply environment to abstract elements *)
    A.change_environment man a env false,
    A.change_environment man b env false


  let assign a v e =
    try
      let e = Texpr1.of_expr a.A.env (expr_to_apron a.A.env e) in
      let v = Var.of_string v in
      let a =
        if Environment.mem_var a.A.env v then a else
        let env = Environment.add a.A.env [||] [|v|] in
        A.change_environment man a env false
      in
      or_bot (A.assign_texpr man a v e None)
    with Manager.Error f ->
      Format.printf "%a@." Manager.print_exclog f;
      failwith "tamere"

  let test a e1 op e2 =
    (* TODO: improve precision for integer comparison *)
    let e1,e2,op = match op with
    | EQ  | EQ_INT -> e1, e2, Tcons1.EQ
    | NEQ | NEQ_INT -> e1, e2, Tcons1.DISEQ
    | GEQ | GEQ_INT -> e1, e2, Tcons1.SUPEQ
    | GT  | GT_INT -> e1, e2, Tcons1.SUP
    | LEQ | LEQ_INT -> e2, e1, Tcons1.SUPEQ
    | LT  | LT_INT -> e2, e1, Tcons1.SUP
    in
    let e = Binary (SUB, e1, e2) in
    let e = Texpr1.of_expr a.A.env (expr_to_apron a.A.env e) in
    let c = Tcons1.array_make a.A.env 1 in
    Tcons1.array_set c 0 (Tcons1.make e op);
    or_bot (A.meet_tcons_array man a c)

  let bwd_assign (a:t) (v:var) (e:expr) : t =
    failwith "Abstract_apron.bwd_assign not implemented"

  let bwd_test (a:t) (e1:expr) (o:cmpop) (e2:expr) : t =
    failwith "Abstract_apron.bwd_test not implemented"


end: ABSTRACT)






(*************)
(* INSTANCES *)
(*************)


module OctF =
  Domain
    (struct
      type lib = Oct.t
      let manager = Oct.manager_alloc ()
    end)
