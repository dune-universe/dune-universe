open Core_kernel

val unique_value :
  'a list ->
  equal:('b -> 'b -> bool) ->
  f:('a -> 'b) ->
  ('b, [> `Empty | `Not_unique of 'b list]) result

val unique_string :
  'a list ->
  f:('a -> string) ->
  string Or_error.t
