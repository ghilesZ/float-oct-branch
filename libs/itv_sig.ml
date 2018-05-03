(*
  An abstract fixpoint solver based on Constraint Programming
  
  Author: Antoine Mine
  Copyright 2014
*)

(* 
   Generic signature for intervals.
   The interface is functional.
 *)

open Bot
open Bound_sig

module type ITV = sig

  (************************************************************************)
  (* TYPES *)
  (************************************************************************)

  (* interval bound (possibly -oo or +oo) *)
  module B : BOUND
  type bound = B.t
        
  (* an interval is a pair of bounds (lower,upper);
     intervals are always non-empty: lower <= upper;
     functions that can return an empty interval return it as Bot
   *)
  type t = bound * bound


  (************************************************************************)
  (* CONSTRUCTORS AND CONSTANTS *)
  (************************************************************************)

  val zero: t           (* {0} *)
  val one: t            (* {1} *)
  val minus_one: t      (* {-1} *)
  val top: t            (* [-oo,+oo] *)
  val zero_one: t       (* [0,1] *)
  val minus_one_zero: t (* [-1,0] *)
  val minus_one_one: t  (* [-1,1] *)
  val positive: t       (* [0,+oo] *)
  val negative: t       (* [-oo,0] *)

  val of_bounds: bound -> bound -> t
  val of_ints: int -> int -> t
  val of_rats: Q.t -> Q.t -> t
  val of_floats: float -> float -> t
  (* [a,b] *)

  val of_bound: bound -> t
  val of_int: int -> t
  val of_rat: Q.t -> t
  val of_float: float -> t
  (* {a} *)

  val hull: bound -> bound -> t
  (* [min a b, max a b] *)


  (************************************************************************)
  (* PRINTING *)
  (************************************************************************)

  val to_string: t -> string
  val output: out_channel -> t -> unit
  val sprint: unit -> t -> string
  val bprint: Buffer.t -> t -> unit
  val pp_print: Format.formatter -> t -> unit


  (************************************************************************)
  (* SET-THEORETIC *)
  (************************************************************************)


  (* operations *)
  (* ---------- *)

  val join: t -> t -> t
  val meet: t -> t -> t bot
      
  (* returns None if the set-union cannot be exactly represented *)
  val union: t -> t -> t option


  (* predicates *)
  (* ---------- *)

  val equal: t -> t -> bool
  val subseteq: t -> t -> bool
  val contains: t -> bound -> bool
  val intersect: t -> t -> bool
  val is_bounded: t -> bool
  val is_singleton: t -> bool
      

  (* mesure *)
  (* ------ *)

  (* length of the intersection (>= 0) *)
  val overlap: t -> t -> bound
      
  val range: t -> bound
  val magnitude: t -> bound


  (* split *)
  (* ----- *)

  val mean: t -> bound
  val split: t -> bound -> (t bot) * (t bot)
  val split_integer: t -> bound -> (t bot) * (t bot)
      

  (************************************************************************)
  (* INTERVAL ARITHMETICS (FORWARD EVALUATION) *)
  (************************************************************************)

  val neg: t -> t
  val abs: t -> t
  val sqrt: t -> t bot

  val add: t -> t -> t
  val sub: t -> t -> t
  val mul: t -> t -> t

  (* return valid values (possibly Bot) + possible division by zero *)
  val div: t -> t -> t bot * bool


  (************************************************************************)
  (* FILTERING (TEST TRANSFER FUNCTIONS) *)
  (************************************************************************)

  (* given two interval arguments, return a subset of each argument 
     by removing points that cannot satisfy the predicate;
     may also return Bot if no point can satisfy the predicate
   *)
      
  val filter_leq: t -> t -> (t * t) bot
  val filter_geq: t -> t -> (t * t) bot
  val filter_lt: t -> t -> (t * t) bot
  val filter_gt: t -> t -> (t * t) bot
  val filter_eq: t -> t -> (t * t) bot
  val filter_neq: t -> t -> (t * t) bot

  (* integer versions *)
  val filter_lt_int: t -> t -> (t * t) bot
  val filter_gt_int: t -> t -> (t * t) bot
  val filter_neq_int: t -> t -> (t * t) bot
      

  (* given the interval argument(s) and the expected interval result of
     a numeric operation, returns refined interval argument(s) where
     points that cannot contribute to a value in the result are
     removed;
     may also return Bot if no point in an argument can lead to a
     point in the result
   *)

  val filter_neg: t -> t -> t bot
  val filter_abs: t -> t -> t bot
  val filter_sqrt: t -> t -> t bot

  val filter_add: t -> t -> t -> (t*t) bot
  val filter_sub: t -> t -> t -> (t*t) bot
  val filter_mul: t -> t -> t -> (t*t) bot
  val filter_div: t -> t -> t -> (t*t) bot


end
