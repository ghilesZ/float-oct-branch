(*
  An abstract fixpoint solver based on Constraint Programming
  
  Author: Antoine Mine
  Copyright 2014
*)

(*
  Adds a bottom element to a data-type.
*)


type 'a bot = Bot | Nb of 'a

val strict_bot: ('a -> 'b bot) -> ('a bot -> 'b bot)
val lift_bot: ('a -> 'b) -> ('a bot -> 'b bot)
val merge_bot2: 'a bot -> 'b bot -> ('a * 'b) bot
val join_bot2: ('a -> 'a -> 'a) -> 'a bot -> 'a bot -> 'a bot

(* maps Bot to a failure *)    
val nobot: 'a bot -> 'a
    
exception Bot_found

(* maps Bot to Bot_found exception *)
val debot: 'a bot -> 'a

(* maps Bot_found back to Bot *)
val rebot: ('a -> 'b) -> 'a -> 'b bot

val bot_to_string: ('a -> string) -> 'a bot -> string
