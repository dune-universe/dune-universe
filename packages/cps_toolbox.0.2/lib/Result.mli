type ('error, 'value) t =
  | Error of 'error
  | Value of 'value

val error : 'error -> ('error, 'value) t
val value : 'value -> ('error, 'value) t
val map : ('a -> 'b) -> ('error, 'a) t -> ('error, 'b) t
