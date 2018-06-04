open Mystdlib

(* unique identifier management *)

type t = int

let get = counter 1

let compare : t -> t -> int = compare

let equal : t -> t -> bool  = (=)

let hash (a:t) = a
