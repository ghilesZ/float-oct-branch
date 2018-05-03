(*
  An abstract fixpoint solver based on Constraint Programming

  Author: Antoine Mine
  Copyright 2015
*)

(*
  Forward abstract interpreter for loop-free program fragments.
  Parameterized by an abstract domain
 *)

open Bot
open Syntax
open Abstract_sig


let neg = function
  | LT -> GEQ | GT -> LEQ | GEQ -> LT | LEQ -> GT | EQ -> NEQ | NEQ -> EQ
  | LT_INT -> GEQ_INT | GT_INT -> LEQ_INT | GEQ_INT -> LT_INT
  | LEQ_INT -> GT_INT | EQ_INT -> NEQ_INT | NEQ_INT -> EQ_INT



(* Path insensitive interpreter *)
(* **************************** *)


module Interpreter(A:ABSTRACT) = struct

  type t = A.t

  (* interpret a statement *)
  let rec interp (ao:t bot) (s:stat) : t bot = match ao,s with
  | Bot, _ ->  Bot
  | _, Block b -> List.fold_left interp ao b
  | Nb a, Assign (v,e) -> A.assign a v e
  | _, If (b,s1,s2) ->
      let a1,a2 = bexpr ao b in
      A.join_bot (interp a1 s1) (interp a2 s2)

  (* filter by a condition, return the true and false environments *)
  and bexpr (ao:t bot) (e:bexpr) : t bot * t bot = match ao,e with
  | Bot, _ -> Bot, Bot
  | _, Not e1 -> let t,f = bexpr ao e1 in f,t
  | _, And (e1,e2) ->
      let t1,f1 = bexpr ao e1
      and t2,f2 = bexpr ao e2 in
      A.meet_bot t1 t2, A.join_bot f1 f2
  | _, Or (e1,e2) ->
      let t1,f1 = bexpr ao e1
      and t2,f2 = bexpr ao e2 in
      A.join_bot t1 t2, A.meet_bot f1 f2
  | _, Rand -> ao, ao
  | Nb a, Cmp (op,e1,e2) ->
     A.test a e1 op e2, A.test a e1 (neg op) e2


  (* backward interpretation of a statement.
     ex:
     - ao:[x=1], s:(x=x+1) -> [x=0] *)
  let rec bwd_interp (a:t) (s:stat) : t =
    match s with
    | Block b -> List.fold_left bwd_interp a b
    | Assign(v,e) -> A.bwd_assign a v e
    | If (b,s1,s2) -> failwith "bwd_interp, not impemented with If constructor"

  (* abstract program:
     - an abstract element for init and one for goal
     - an abstract transfer function for the body *)
  let prog (p:prog) : t * t * (t -> t) =
    let debot msg x = match x with
    | Nb y -> y
    | Bot -> failwith ("unexpected bottom in "^msg)
    in
    debot "init" (interp (Nb A.init) p.init),
    debot "goal" (interp (Nb A.init) p.goal),
    (fun x ->
      (* apply loop iteration *)
      let r = debot "body" (interp (Nb x) p.body) in
      (* remove added variables that are not in the argument *)
      fst (A.unify_meet r x)
    )

end



(* Path sensitive interpreter *)
(* ************************** *)

module PSInterpreter(A:ABSTRACT) = struct

  type t = A.t

  let map_nobot (f:'a -> 'b bot) (l:'a list) : 'b list =
    List.fold_left
      (fun acc b -> match b with
      | Bot -> acc
      | Nb a -> a::acc
      ) [] (List.rev_map f l)


  let join_list (a:t list) : t bot =
    List.fold_left
      (fun acc x -> A.join_bot acc (Nb x))
      Bot a


  (* interpret a statement *)
  let rec interp (a:t list) (s:stat) : t list = match s with
  | Block b ->
      List.fold_left interp a b

  | Assign (v,e) ->
      map_nobot (fun x -> A.assign x v e) a

  | If (b,s1,s2) ->
      let a1,a2 = bexpr a b in
      (interp a1 s1)@(interp a2 s2)


  (* filter by a condition, return the true and false environments *)
  and bexpr (a:t list) (e:bexpr) : t list * t list = match e with
  | Not e1 ->
      let t,f = bexpr a e1 in f,t

  | And (e1,e2) ->
      let t1,f1 = bexpr a e1 in
      let t1t2,t1f2 = bexpr t1 e2 in
      t1t2, f1@t1f2

  | Or (e1,e2) ->
      let t1,f1 = bexpr a e1 in
      let f1t2,f1f2 = bexpr f1 e2 in
      t1@f1t2, f1f2

  | Cmp (op,e1,e2) ->
      map_nobot (fun x -> A.test x e1 op e2) a,
      map_nobot (fun x -> A.test x e1 (neg op) e2) a

  | Rand ->
      a, a

  (* abstract program *)
  let prog (p:prog) : t * t * (t -> t list) =
    let debot msg x = match x with
    | Nb y -> y
    | Bot -> failwith ("unexpected bottom in "^msg)
    in
    debot "init" (join_list (interp [A.init] p.init)),
    debot "goal" (join_list (interp [A.init] p.goal)),
    (fun x ->
      (* apply loop iteration *)
      let rs = interp [x] p.body in
      (* remove added variables that are not in the argument *)
      List.map (fun r -> fst (A.unify_meet r x)) rs
    )

end
