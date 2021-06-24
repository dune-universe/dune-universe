type 'a tree =
  | Null
  | Node of int * int * 'a * 'a tree * 'a tree

type 'a compare = 'a -> 'a -> Order.t

val null : 'a tree
val node : int -> int -> 'a -> 'a tree -> 'a tree -> 'a tree
val fold : 'r -> (int -> int -> 'a -> 'r -> 'r -> 'r) -> 'a tree -> 'r
val map : ('a -> 'b) -> 'a tree -> 'b tree
val get_count : 'a tree -> int
val get_height : 'a tree -> int
val insert : 'a compare -> 'a -> 'a tree -> 'a tree
val remove : 'a compare -> 'a -> 'a tree -> 'a tree
val is_member :
  'a compare -> 'a -> 'a tree ->
  (unit -> 'r) -> (unit -> 'r) -> 'r
val get_member : int -> 'a tree -> (unit -> 'r) -> ('a -> 'r) -> 'r
val get_leftmost : 'a tree -> (unit -> 'r) -> ('a -> 'r) -> 'r
val get_rightmost : 'a tree -> (unit -> 'r) -> ('a -> 'r) -> 'r
val to_list : 'a tree -> 'a list
val from_list : 'a list -> 'a tree
