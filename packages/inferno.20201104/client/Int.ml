(* Integers as an ordered type. *)

type t =
    int

let compare (x : int) (y : int) =
  if x < y then -1
  else if x = y then 0
  else 1

