(*
  An abstract fixpoint solver based on Constraint Programming

  Author: Antoine Mine
  Copyright 2014
*)

(*
  Boxes: products of (non-empty) intervals.

  Boxes are immutable.

  Internally, we represent boxes as (functional) maps from
  (possibily non-contiguous) integers (denoting variable indices)
  to non-empty intervals. We benefit from the automatic sharing
  between arguments and result when modifing a small set of intervals,
  and from accelerated binary operations when both arguments share some
  part of the map.

 *)


open Bot
open Bound_sig
open Itv_sig
open Abstract_sig
open Syntax



(*******************)
(* GENERIC FUNCTOR *)
(*******************)


module Box(I:ITV) = (struct


  (************************************************************************)
  (* TYPES *)
  (************************************************************************)


  (* interval and bound inheritance *)
  module I = I
  module B = I.B
  type bound = B.t
  type i = I.t


  (* maps from variables *)
  module Env = Mapext.SMap

  (* maps each variable to a (non-empty) interval *)
  type t = i Env.t

  (* boxes split along variables *)
  type dir = var


  (************************************************************************)
  (* PRINTING *)
  (************************************************************************)


  let bprint b a =
    let first = ref true in
    Env.iter
      (fun v i ->
        Printf.bprintf b "%s%s:%a" (if !first then "" else " ") v I.bprint i;
        first := false
      ) a

  let to_string a =
    let b = Buffer.create 16 in
    bprint b a;
    Buffer.contents b

  let sprint () a = to_string a
  let output chan a = output_string chan (to_string a)
  let pp_print fmt a = Format.pp_print_string fmt (to_string a)


  let to_polygon a v1 v2 =
    let (l1,h1),(l2,h2) = Env.find v1 a, Env.find v2 a in
    let l1,h1 = B.to_float_down l1, B.to_float_up h1
    and l2,h2 = B.to_float_down l2, B.to_float_up h2 in
    [l1,l2; h1,l2; h1,h2; l1,h2]



  (************************************************************************)
  (* SET-THEORETIC *)
  (************************************************************************)


  (* NOTE: all binary operations assume that both boxes are defined on
     the same set of variables;
     otherwise, an Invalid_argument exception will be raised
   *)



  (* operations *)
  (* ---------- *)

  let join (a:t) (b:t) =
    Env.map2z (fun _ x y -> I.join x y) a b

  let meet (a:t) (b:t) : t bot =
    rebot (Env.map2z (fun _ x y -> debot (I.meet x y)) a) b


  let join_bot (a:t bot) (b:t bot) : t bot =
    match a,b with
    | _,Bot -> a | Bot,_ -> b
    | Nb aa, Nb bb -> Nb (join aa bb)

  let meet_bot (a:t bot) (b:t bot) : t bot =
    match a,b with
    | Nb aa, Nb bb -> meet aa bb
    | _ -> Bot

  (* set difference *)
  let diff (a:t) (b:t) : t list =
    Env.fold2 (fun v i_a i_b acc ->
        let s = I.diff i_a i_b in
        List.fold_left (fun acc e ->
            (Env.add v e a)::acc
          ) acc s
      ) a b []

  (* predicates *)
  (* ---------- *)


  let equal (a:t) (b:t) : bool =
    Env.for_all2z (fun _ x y -> I.equal x y) a b

  let subseteq (a:t) (b:t) : bool =
    Env.for_all2z (fun _ x y -> I.subseteq x y) a b

  let intersect (a:t) (b:t) : bool =
    Env.for_all2z (fun _ x y -> I.intersect x y) a b

  let intersect_nondegenerate (a:t) (b:t) : bool =
    Env.for_all2z (fun _ x y -> I.intersect x y) a b



  (* mesure *)
  (* ------ *)


  (* total number of variables *)
  let dimension (a:t) : int =
    Env.cardinal a

  (* number of non-singleton variables *)
  let nondegenerate_dimension (a:t) : int =
    Env.fold (fun _ x n -> if I.is_singleton x then n else n+1) a 0

  (* volume *)
  let volume (a:t) : bound =
    Env.fold (fun _ x v -> B.mul_up (I.range x) v) a B.one

  (* exact volume computation, using rationals *)
  let exact_volume (a:t) : Q.t =
    Env.fold
      (fun _ (l,h) v ->  Q.mul (Q.sub (B.to_rat h) (B.to_rat l)) v) a Q.one

  let degenerate (a:t) : bool =
    Env.exists (fun _ x -> B.equal (I.range x) B.zero) a


  (* volume, considering only non-singleton dimensions;
     for instance, the volume of a point is always 1
   *)
  let volume_ext (a:t) : bound =
    Env.fold
      (fun _ x v ->
        let r = I.range x in
        if B.sign r <= 0 then v else B.mul_up r v
      ) a B.one


  (* volume of the intersection *)
  let overlap (a:t) (b:t) : bound =
    match meet a b with
    | Bot -> B.zero
    | Nb x -> volume_ext x



  (* split *)
  (* ----- *)


  let is_integer var =
    var.[String.length var - 1] = '%'


  (* variable with maximal range *)
  let max_range (a:t) : var * i =
    Env.fold
      (fun v i (vo,io) ->
        if B.geq (I.range i) (I.range io) then v,i else vo,io
      ) a (Env.min_binding a)


  (* split along a specified variable *)
  let split (a:t) (v:var) (b:bound) : (t bot) * (t bot) =
    let i = Env.find v a in
    let i1,i2 = (if is_integer v then I.split_integer else I.split) i b in
    lift_bot (fun ii -> Env.add v ii a) i1,
    lift_bot (fun ii -> Env.add v ii a) i2

  (* maximal range of variables *)
  let size (a:t) : B.t =
    Env.fold (fun _ i r -> B.max r (I.range i)) a B.zero



  (************************************************************************)
  (* ABSTRACT OPERATIONS *)
  (************************************************************************)


  (* initial box: no variable at all *)
  let init : t =
    Env.empty

  let add_var (a:t) (v:var) (i:i) : t =
    Env.add v i a

  let get_var_range (a:t) (v:var) : i =
    try Env.find v a
    with Not_found -> failwith ("variable not found: "^v)

  let set_var_range (a:t) (v:var) (i:i) : t =
    Env.add v i a

  let remove_var (a:t) (v:var) : t =
    Env.remove v a

  let get_variables (a:t) : var list =
    List.rev (Env.fold (fun v _ acc -> v::acc) a [])

  let nonpoint_variables (a:t) : var list =
    List.rev
      (Env.fold (fun v i acc -> if I.is_singleton i then acc else v::acc) a [])



  (* trees with nodes annotated with evaluation *)
  type bexpr =
    | BUnary of unop * bexpri
    | BBinary of binop * bexpri * bexpri
    | BVar of var
    | BCst of i

  and bexpri = bexpr * i


  (* interval evaluation of an expression;
     returns the interval result but also an expression tree annotated with
     intermediate results (useful for test transfer functions

     errors (e.g. division by zero) return no result, so:
     - we raies Bot_found in case the expression only evaluates to error values
     - otherwise, we return only the non-error values
   *)
  let rec eval (a:t) (e:expr) : bexpri =
    match e with
    | Var v ->
        let r =
          try Env.find v a
          with Not_found -> failwith ("variable not found: "^v)
        in
        BVar v, r
    | Cst (l,h) ->
        let r = I.of_rats l h in
        BCst r, r
    | Unary (o,e1) ->
        let _,i1 as b1 = eval a e1 in
        let r = match o with
        | NEG -> I.neg i1
        | ABS -> I.abs i1
        | SQRT -> debot (I.sqrt i1)
        in
        BUnary (o,b1), r
    | Binary (o,e1,e2) ->
        let _,i1 as b1 = eval a e1
        and _,i2 as b2 = eval a e2 in
        let r = match o with
        | ADD -> I.add i1 i2
        | SUB -> I.sub i1 i2
        | DIV -> debot (fst (I.div i1 i2))
        | MUL ->
            let r = I.mul i1 i2 in
            if e1=e2 then
              (* special case: squares are positive *)
              debot (I.meet r I.positive)
            else r
        in
        BBinary (o,b1,b2), r


  (* returns a box included in its argument, by removing points such that
     the evaluation is not in the interval;
     not all such points are removed, due to interval abstraction;
     iterating eval and refine can lead to better reults (but is more costly);
     can raise Bot_found
   *)
  let rec refine (a:t) (e:bexpr) (x:i) : t =
    match e with
    | BVar v ->
        (try Env.add v (debot (I.meet x (Env.find v a))) a
        with Not_found -> failwith ("variable not found: "^v))
    | BCst i -> ignore (debot (I.meet x i)); a
    | BUnary (o,(e1,i1)) ->
        let j = match o with
        | NEG -> I.filter_neg i1 x
        | ABS -> I.filter_abs i1 x
        | SQRT -> I.filter_sqrt i1 x
        in
        refine a e1 (debot j)
    | BBinary (o,(e1,i1),(e2,i2)) ->
        let j = match o with
        | ADD -> I.filter_add i1 i2 x
        | SUB -> I.filter_sub i1 i2 x
        | MUL -> I.filter_mul i1 i2 x
        | DIV -> I.filter_div i1 i2 x
        in
        let j1,j2 = debot j in
        refine (refine a e1 j1) e2 j2



  (* assignment transfer function;
     can return Bot, in case the expression only evaluates to error values
   *)
  let assign (a:t) (v:var) (e:expr) : t bot =
    rebot (fun a -> Env.add v (snd (eval a e)) a) a


  (* test transfer function *)
  let test (a:t) (e1:expr) (o:cmpop) (e2:expr) : t bot =
    let (b1,i1), (b2,i2) = eval a e1, eval a e2 in
    let j = match o with
      | EQ | EQ_INT -> I.filter_eq i1 i2
      | LEQ | LEQ_INT -> I.filter_leq i1 i2
      | GEQ | GEQ_INT -> I.filter_geq i1 i2
      | NEQ -> I.filter_neq i1 i2
      | NEQ_INT -> I.filter_neq_int i1 i2
      | GT -> I.filter_gt i1 i2
      | GT_INT -> I.filter_gt_int i1 i2
      | LT -> I.filter_lt i1 i2
      | LT_INT -> I.filter_lt_int i1 i2
    in
    rebot
      (fun a ->
        let j1,j2 = debot j in
        refine (refine a b1 j1) b2 j2
      ) a


  (* remove from a variable not in b, and from b variables not in a *)
  let unify_meet (a:t) (b:t) : t * t =
    Env.fold2zo
      (fun v _ (a,b) -> Env.remove v a, b)
      (fun v _ (a,b) -> a, Env.remove v b)
      (fun _ _ _ ab -> ab)
      a b (a,b)

  let bwd_assign (a:t) (v:var) (e:expr) : t =
    failwith "Abstract_box.bwd_assign not implemented"

  let bwd_test (a:t) (e1:expr) (o:cmpop) (e2:expr) : t =
    failwith "Abstract_box.bwd_test not implemented"


end: ABSTRACT)



(*************)
(* INSTANCES *)
(*************)


module BoxF = Box(Itv.ItvF)
module BoxQ = Box(Itv.ItvQ)
