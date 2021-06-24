type 'a set
type 'a compare = 'a -> 'a -> Order.t
val empty : 'a set
val is_empty : 'a set -> bool
val is_member : 'a compare -> 'a -> 'a set -> (unit -> 'r) -> (unit -> 'r) -> 'r
val get_member : int -> 'a set -> (unit -> 'r) -> ('a -> 'r) -> 'r
val get_member_unsafe : int -> 'a set -> 'a
val size : 'a set -> int
val add : 'a compare -> 'a -> 'a set -> 'a set
val remove : 'a compare -> 'a -> 'a set -> 'a set
val to_list : 'a set -> 'a list
val from_list : 'a list -> 'a set
val fold : 'r -> ('a -> 'r -> 'r) -> 'a set -> 'r
val map : ('a -> 'b) -> 'a set -> 'b set
val union : 'a compare -> 'a set -> 'a set -> 'a set
val difference : 'a compare -> 'a set -> 'a set -> 'a set
val intersection : 'a compare -> 'a set -> 'a set -> 'a set
val has_intersection : 'a compare -> 'a set -> 'a set
  -> (unit -> 'r) -> (unit -> 'r) -> 'r
val first : 'a set -> (unit -> 'r) -> ('a -> 'r) -> 'r
val first_unsafe : 'a set -> 'a
val last : 'a set -> (unit -> 'r) -> ('a -> 'r) -> 'r
val last_unsafe : 'a set -> 'a
val compare : ('a -> 'a -> Order.t) -> 'a set -> 'a set -> Order.t
