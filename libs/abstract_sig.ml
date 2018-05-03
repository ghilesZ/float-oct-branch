(*
  An abstract fixpoint solver based on Constraint Programming

  Author: Antoine Mine
  Copyright 2014
*)


(*
  Generic signature for abstract set of points, such as boxes.
  The interface is functional.
 *)


open Bot
open Bound_sig
open Itv_sig
open Syntax


module type ABSTRACT = sig


  (************************************************************************)
  (* TYPES *)
  (************************************************************************)

  (* underlying bound and interval type *)
  module I : ITV
  module B = I.B
  type bound = B.t
  type i = I.t

  (* abstract set of points *)
  type t

  (* abstract type of directions, along which a split can occur;
     which directions are supported depend on the abstract domain
   *)
  type dir


  (************************************************************************)
  (* PRINTING *)
  (************************************************************************)

  val to_string: t -> string
  val output: out_channel -> t -> unit
  val sprint: unit -> t -> string
  val bprint: Buffer.t -> t -> unit
  val pp_print: Format.formatter -> t -> unit


  (* special to print svg *)

  val to_polygon: t -> var -> var -> (float * float) list



  (************************************************************************)
  (* SET-THEORETIC *)
  (************************************************************************)


  (* NOTE: all binary operations assume that both boxes are defined on
     the same set of variables;
     otherwise, an Invalid_argument exception will be raised
   *)

  val join: t -> t -> t
  val meet: t -> t -> t bot
  val join_bot: t bot -> t bot -> t bot
  val meet_bot: t bot -> t bot -> t bot

  val equal: t -> t -> bool
  val subseteq: t -> t -> bool
  val intersect: t -> t -> bool

  (* whether the intersection is non-empty and has non-zero volume *)
  val intersect_nondegenerate: t -> t -> bool

  (* total number of variables *)
  val dimension: t -> int

  (* number of non-singleton variables *)
  val nondegenerate_dimension: t -> int

  (* (approximate) volume *)
  val volume: t -> bound

  (* exact volume, may raise a failure if not supported *)
  val exact_volume: t -> Q.t

  (* whether the volume is zero *)
  val degenerate: t -> bool

  (* (approximate) volume, taking into account only non-singleton dimensions *)
  val volume_ext: t -> bound

  (* (approximate) volume of the intersection *)
  val overlap: t -> t -> bound

  (* direction with maximal range, and its range *)
  val max_range: t -> dir * i

  (* splits along a specified direction and value *)
  val split: t -> dir -> bound -> (t bot) * (t bot)

  (* maximal range in all direction *)
  val size: t -> bound


  (************************************************************************)
  (* ABSTRACT OPERATIONS *)
  (************************************************************************)


  (* initial abstract state: no variable at all *)
  val init : t

  (* adds a variable, initialized to the given range *)
  val add_var: t -> var -> I.t -> t

  (* deletes a variable *)
  val remove_var: t -> var -> t

  (* get the range of a variable *)
  val get_var_range: t -> var -> I.t

  (* get the list of variables *)
  val get_variables: t -> var list

  (* sets the range of a variable *)
  val set_var_range: t -> var -> I.t -> t

  (* project both elements on the intersection of their variable set *)
  val unify_meet: t -> t -> t * t

  (* assignment transfer function;
     can return Bot, in case the expression only evaluates to error values
  *)
  val assign: t -> var -> expr -> t bot

  (* test transfer function *)
  val test: t -> expr -> cmpop -> expr -> t bot

  (* backward version of the abstract operations *)
  (***********************************************)
  val bwd_assign: t -> var -> expr -> t

  val bwd_test: t -> expr -> cmpop -> expr -> t

end
