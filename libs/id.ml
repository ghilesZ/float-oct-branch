(* unique identifier management *)

type t = int

let get =
  let next = ref 0 in
  fun () -> incr next; !next

let compare (a:t) (b:t) = compare a b

let equal (a:t) (b:t) = a=b

let hash (a:t) = a
