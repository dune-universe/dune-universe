(* for pretty print BatMap.t *)
type ('k, 'v) lmap = ('k * 'v) list
[@@deriving show]

module Map = struct
  include BatMap
  let diffkeys m ks =
    List.fold_left (fun m k -> BatMap.remove k m) m ks
  let find_opt k m =
    try
      Some (BatMap.find k m)
    with Not_found ->
      None
  let pp f g fmt t =
    pp_lmap f g fmt @@ BatList.of_enum @@ BatMap.enum t
end

module Set = BatSet
module Enum = BatEnum
module List = BatList
module Array = BatArray
module String = BatString

type ('k, 'v) map = ('k, 'v) Map.t
[@@deriving show]

type 'e set = 'e Set.t

let fst (a, _) = a
let snd (_, a) = a

let (<.>) a b = (a, b)

let (>>) f g = fun x -> f (g x)
let (<<) f g = fun x -> g (f x)

let flip f a b = f b a