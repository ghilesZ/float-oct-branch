(*
  An abstract fixpoint solver based on Constraint Programming
  
  Author: Antoine Mine
  Copyright 2015
 *)


(* NOT USED *)


(* 
   Tree-data structure to handle abstract partitioning.
   Mutable data-structure.
 *)


open Bot
open Bound_sig
open Itv_sig
open Abstract_sig
open Syntax

  
module Id = struct
  type t = int
  let next = ref 0
  let get () = incr next; !next
  let compare (a:t) (b:t) = compare a b
  let equal (a:t) (b:t) = a=b
  let hash (a:t) = a
end

module IdHash = Hashtbl.Make(Id)
module IdSet  = Set.Make(Id)
module IdMap  = Mapext.Make(Id)
  

module KDTree(A:ABSTRACT) = struct

  module I = A.I
  module B = A.B
  type bound = A.bound

  type id = Id.t

  (* 
     The partition is defined using a tree, by recursive split.
     An internal node encompases its children.
     The partition per se is composed of the leaves of the tree.
   *)
        

  type node = {
      id: id;
      space: A.t;
      mutable childs: node list; (* empty for leaves *)
    }

  type t = {
      root: node;
      leaves: node IdHash.t;
    }


        
  let create (universe:A.t) : t =
    let root = { id = Id.get(); space = universe; childs = []; } in
    let leaves = IdHash.create 16 in
    IdHash.add leaves root.id root;
    { root; leaves; }      


  (* returns the list of partitions that intersect x;
     also returns true if some parts of x do not lie in any partition
   *)
  let cover (tree:t) (x:A.t) : id list * bool =
    let rec search xx acc node =
      match A.meet xx node.space with
      | Bot -> acc
      | Nb xx ->
          if node.childs = [] then node.id::acc (* leaf *)
          else List.fold_left (search xx) acc node.childs (* split *)
    in
    (search x [] tree.root),
    (A.subseteq x tree.root.space)

      
  (* space occupied by a partition *)
  let get_space (tree:t) (id:id) : A.t =
    try (IdHash.find tree.leaves id).space
    with Not_found -> invalid_arg "unknown partition in KDTree"
        
  (* utility *)
  let mk_leaf (tree:t) (space:A.t) : node =
    let node = { id = Id.get(); space; childs = []; } in
    IdHash.add tree.leaves node.id node;
    node

  (* split a partition along a variable and bound *)
  let split (tree:t) (id:id) (dir:A.dir) (bound:bound) : id*id =
    let node =
      try IdHash.find tree.leaves id
      with Not_found -> invalid_arg "unknown partition in KDTree"
    in
    match A.split node.space dir bound with
    | Bot,_ | _,Bot -> invalid_arg "cannot split node in KDTree"
    | Nb s1, Nb s2 ->
        let n1,n2 = mk_leaf tree s1, mk_leaf tree s2 in
        node.childs <- [n1;n2];
        IdHash.remove tree.leaves id;
        n1.id, n2.id
      
end
