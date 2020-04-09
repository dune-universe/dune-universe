open Core

type t = Wordset.t String.Map.t 

let empty = String.Map.empty

let get vs k = match Map.find vs k with
  | Some w -> w
  | None -> Wordset.empty

let set vs k w = Map.set vs ~key:k ~data:w
