(*
  An abstract fixpoint solver based on Constraint Programming
  
  Author: Antoine Mine
  Copyright 2015
*)


(* Arbitrary precision rationals, based on Zarith.
   Implements the Bound_sig.BOUND interface.
 *)

type t = Q.t

let compare = Q.compare
let equal = Q.equal
let lt = Q.lt
let gt = Q.gt
let leq = Q.leq
let geq = Q.geq
let neq x y = not (Q.equal x y)
let min = Q.min
let max = Q.max
let sign = Q.sign

let validate x =
  assert (x <> Q.undef);
  x
  

let of_int_up = Q.of_int
let of_int_down = Q.of_int
let of_z_up = Q.of_bigint
let of_z_down = Q.of_bigint
let of_float_up x = validate (Q.of_float x)
let of_float_down = of_float_up
let of_rat_up q : t = validate q
let of_rat_down = of_rat_up
    
let of_string_up x = validate (Q.of_string x)
let of_string_down = of_string_up

let to_string = Q.to_string
let output = Q.output
let sprint = Q.sprint
let bprint = Q.bprint
let pp_print = Q.pp_print

let to_rat x : Q.t = x

(* Note: adds 0. to favor positive 0 *)
let to_float_up a =
   ((Z.to_float a.Q.num) /. (-.(Z.to_float (Z.neg a.Q.den)))) +. 0.

let to_float_down a =
  (-.((Z.to_float (Z.neg a.Q.num)) /. (Z.to_float a.Q.den))) +. 0.


    
(* classification *)    
      
type kind = FINITE | MINF | INF | INVALID
  
let classify (x:t) : kind =
  if x = Q.undef then INVALID
  else if x = Q.inf then INF
  else if x = Q.minus_inf then MINF
  else FINITE

let zero = Q.zero
let one = Q.one
let minus_one = Q.minus_one
let inf = Q.inf
let minus_inf = Q.minus_inf
let two = Q.of_int 2
    
let neg = Q.neg
let abs = Q.abs
    
let add_up = Q.add
let sub_up = Q.sub
let mul_up = Q.mul
let div_up = Q.div

let add_down = Q.add
let sub_down = Q.sub
let mul_down = Q.mul
let div_down = Q.div

(* lower approximation of a square root *)
let sqrt_down (a:Q.t) : Q.t =
  if Q.sign a < 0 then invalid_arg "square root of negative number";
  (* sqrt (a/b) = sqrt (ab/bb) = sqrt (ab) / b *)
  Q.make (Z.sqrt (Z.mul a.Q.num a.Q.den)) a.Q.den
    
(* upper approximation of a square root *)
let sqrt_up (a:Q.t) : Q.t =
  if Q.sign a < 0 then invalid_arg "square root of negative number";
  let s,r = Z.sqrt_rem (Z.mul a.Q.num a.Q.den) in
  Q.make (if Z.sign r > 0 then Z.succ s else s) a.Q.den
  
let floor a = Q.of_bigint (Z.fdiv a.Q.num a.Q.den)
let ceil  a = Q.of_bigint (Z.cdiv a.Q.num a.Q.den)
    
