(***************)
(*  shortcuts  *)
(***************)

let foi = float_of_int
let iof = int_of_float
let soi = string_of_int
let ios = int_of_string
let sof = string_of_float
let fos = float_of_string
let sob = string_of_bool
let bos = bool_of_string
let fff = Format.fprintf

(*************************)
(* affectation operators *)
(*************************)

let ( += )  a b = a := !a + b
let ( +=. ) a b = a := !a +. b
let ( -= )  a b = a := !a - b
let ( -=. ) a b = a := !a -. b
let ( *= )  a b = a := !a * b
let ( *=. ) a b = a := !a *. b
let ( /= )  a b = a := !a / b
let ( /=. ) a b = a := !a /. b
let ( %= )  a b = a := !a mod b
let ( &&= ) a b = a := !a && b
let ( ||= ) a b = a := !a || b

(* \x -> x *)
let identity = fun x -> x

(**************************************)
(*               other                *)
(**************************************)

(** returns a closure *)
let closure f a =
  let tmp = ref a in
  fun () -> tmp := f !tmp; !tmp

(** closure operator *)
let ( %% ) f a = closure f a

(** counter x returns a counter starting at x *)
let counter x = succ %% (x-1)

(** composition: (f -| g) x = f (g x) *)
let ( -| ) f g = fun x -> g x |> f

(** fixpoint computation *)
let until f x0 pred =
  let rec aux x =
    let x' = f x in
    if pred x x' then x else aux x'
  in aux x0

(** fixpoint computation with equality *)
let fixpoint f x0 = until f x0 (=)

(** iterate n times f e*)
let iter f e n =
  let rec loop i e =
    if (i < n) then f e
    else Invalid_argument "must iterate more than zero times" |> raise
  in loop 0 e

(** iter but different treatment for last element *)
let iterbutlast ?last allbutlast from dest =
  for i = from to (dest-1) do allbutlast () done;
  match last with
  | None -> ()
  | Some f -> f ()

(** iterate n times f i e*)
let iteri f e n =
  let rec loop i e =
    if (i < n) then f i e
    else Invalid_argument "must iterate more than zero times" |> raise
  in loop 0 e

(** same as iter but last fut function with int parameter *)
let iteributlast ?last allbutlast from dest =
  for i = from to (dest-1) do allbutlast i done;
  match last with
  | None -> ()
  | Some f -> f dest

(** if then *)
let though b f = if b then f ()

(** if not then *)
let unless = though -| not

(** do f a then return a; useful for printing when testing *)
let return_after (f:'a -> unit) a = f a; a

let ( |>- ) a f = return_after f a

module Array = struct

  include Array

  (** dichotomic search over a sorted array :
      dichotomic_search p [|a1; ...; an|] searchs an element e in a sorted array
      such that p e = 0;
      The p function must return 0 if its arguments is the targeted one,
      a positive integer if the targeted element should appear after the
      current element in the array, and a negative integer otherwise.
      It raises Not_found if no such element exists in the array.
      Warning: unspecified behaviour over non-sorted arrays, in particular the
      function is not guaranteed to terminate. *)
  let dichotomic_search pred arr =
    let rec aux i1 i2 =
      if i1 > i2 then raise Not_found
      else
        let mid_idx = i2-(i2-i1)/2 in
        let mid_elm = arr.(mid_idx) in
        match pred mid_elm with
        | 0  -> mid_elm
        | x  -> if x < 0 then aux i1 (mid_idx-1)
                else aux (mid_idx+1) i2
    in aux 0 ((length arr) - 1)

  (** pick a random element of an array *)
  let pick r =
    let i = Random.int (Array.length r) in
    r.(i)

  (**swap of two elements of an array*)
  let swap t i j =
    let tmp = t.(i) in
    t.(i) <- t.(j);
    t.(j) <- tmp

  (** knuth-fisher-yates shuffle for arrays *)
  let shuffle t =
    let n = Array.length t in
    for i = n-1 downto 1 do
      let k = Random.int (i+1) in
      swap t i k
    done
end

module List = struct

  include List

  (**************************************)
  (*           list building            *)
  (**************************************)

  (** range 1 `To 3 = [1; 2; 3]. range 3 `Downto 1 = [3; 2; 1]. *)
  let range i dir j =
    let op,first = match dir with
      | `To when i < j -> pred,j
      | `Downto when i > j -> succ,j
      | `Step s when i < j && s > 0 -> (fun a -> a - s),(j-((j-i) mod s))
      | `Step s when i > j && s < 0 -> ((+) (-s)),(j+((i-j) mod s))
      | _ when i = j -> (fun x -> x),j
      | _ -> invalid_arg (Printf.sprintf "range: %d %d" i j)
    in
    let rec loop acc k =
      if i = k then k :: acc
      else loop (k :: acc) (op k)
    in loop [] first

  (*****************************************)
  (* fold functions with early termination *)
  (*****************************************)

  (** fold function with early termination: absorb defines an element s.t,
    if it is found during the computation it is returned immediatelly. *)
  let fold ?absorb f acc l =
    match absorb with
    | None -> List.fold_left f acc l
    | Some s ->
       let exception Stop in
       let f' acc x =
         let res = f acc x in
         if res = s then raise Stop
         else res
       in
       try List.fold_left f' acc l
       with Stop -> s

  (** fold, using the first element of the list as a default value. *)
  let reduce ?absorb f = function
    | [] -> raise (Invalid_argument "can't reduce an empty list")
    | h::tl -> fold ?absorb:absorb f h tl

  let best cmp x = reduce (fun a b -> if cmp a b > 0 then a else b) x

  let worst cmp x = reduce (fun a b -> if cmp a b < 0 then a else b) x

  (** pick a random element of a list *)
  let pick l = Random.int (List.length l) |> List.nth l

  (** knuth-fisher-yates shuffle of list *)
  let shuffle l =
    let t = Array.of_list l in
    Array.shuffle t;
    Array.to_list t

  (** unsafe pattern matching : hack to avoid warning in let [a;b] = l *)
  let destroy = function
    | a::b -> a,b
    | _ -> failwith "cant destroy an empty list"
end

(** operator for range i `To j *)
let ( ++ ) a b = List.range a `To b

(** operator for range i `Downto j *)
let ( -- ) a b = List.range a `Downto b

module Format = struct

  include Format

  (* few common separators *)
  let comma_sep = fun fmt () -> Format.fprintf fmt ","
  let newline_sep = fun fmt () -> Format.fprintf fmt "\n"
  let space_sep = fun fmt () -> Format.fprintf fmt "\n"

end
