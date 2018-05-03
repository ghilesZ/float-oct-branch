(*
  An abstract fixpoint solver based on Constraint Programming
  
  Author: Antoine Mine
  Copyright 2015
 *)


(* NOT USED *)

(*
  List with append and removal at both ends.

  Functional data-structure.
 *)



(* An element a represents the list a.head@(rev b.tail). *)
type 'a t =
    { head: 'a list;
      tail: 'a list;
    }


let empty () : 'a t =
  { head = []; tail = []; }

let of_list (l:'a list) : 'a t =
  { head = l; tail = []; }
    
let insert_head (a:'a) (l:'a t) : 'a t =
  { l with head = a::l.head; }

let insert_tail (l:'a t) (a:'a) : 'a t =
  { l with tail = a::l.tail; }

let append_head (a:'a list) (l:'a t) : 'a t =
  { l with head = a@l.head; }

let append_tail (l:'a t) (a:'a list) : 'a t =
  { l with tail = List.rev_append a l.tail; }

let is_empty (l:'a t) : bool =
  l.head = [] && l.tail = []

let length (l:'a t) : int =
  (List.length l.head) + (List.length l.tail)


(* utility to split a list in two;
   returns a,b such that l = a@b, |a| = |l|/2, b = l-|l|/2
*)
let halve (l:'a list) : 'a list * 'a list =
  let rec doit a b nb =
    if nb = 0 then List.rev a, b else match b with
    | x::y -> doit (x::a) y (nb-1)
    | [] -> failwith "impossible"
  in
  doit [] l ((List.length l)/2)

    
(* extracts the first element; 
   returns the element and the rest of the list
 *)    
let head (l:'a t) : 'a * 'a t =
  (* ensures that head is not empty, if possible *)
  let l =
    if l.head <> [] then l else
    let tail, head = halve l.tail in
    { head = List.rev head; tail; }
  in  
  match l.head with
  | a::b -> a, { l with head = b; }
  | [] -> assert(l.tail=[]); invalid_arg "Dequeue: empty"

    
(* extracts the last element; 
   returns the element and the rest of the list
 *)    
let tail (l:'a t) : 'a * 'a t =
  (* ensures that tail is not empty, if possible *)
  let l =
    if l.tail <> [] then l else
    let head, tail = halve l.head in
    { head; tail = List.rev tail; }
  in  
  match l.tail with
  | a::b -> a, { l with tail = b; }
  | [] -> assert(l.head=[]); invalid_arg "Dequeue: empty"
