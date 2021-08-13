type ('key, 'value) map

val empty : ('key, 'value) map
val size : ('key, 'value) map -> int
val fold : 'r -> ('key -> 'value -> 'r -> 'r) -> ('key, 'value) map -> 'r
val map : ('a -> 'b) -> ('key, 'a) map -> ('key, 'b) map
val contains : ('key -> 'key -> Order.t) -> 'key  -> ('key, 'value) map
  -> (unit -> 'r) -> (unit -> 'r) -> 'r
val insert : ('key -> 'key -> Order.t) -> 'key -> 'value -> ('key, 'value) map
  -> ('key, 'value) map
val remove : ('key -> 'key -> Order.t) -> 'key -> ('key, 'value) map
  -> ('key, 'value) map
val lookup : ('key -> 'key -> Order.t) -> 'key -> ('key, 'value) map
  -> (unit -> 'r) -> ('value -> 'r) -> 'r
val lookup_unsafe : ('key -> 'key -> Order.t) -> 'key -> ('key, 'value) map
  -> 'value
val entries : ('key, 'value) map -> ('key * 'value) list
val keys : ('key, 'value) map -> 'key list
val values : ('key, 'value) map -> 'value list
val from_entries : ('key * 'value) list -> ('key, 'value) map
