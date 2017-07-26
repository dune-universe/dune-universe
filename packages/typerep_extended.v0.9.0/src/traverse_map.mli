open! Core_kernel

val option : 'a option -> f:('a -> 'a) -> 'a option
val array  : 'a array  -> f:('a -> 'a) -> 'a array
