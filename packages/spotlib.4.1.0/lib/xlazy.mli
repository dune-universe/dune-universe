open Lazy
include Monad.T with type 'a t := 'a Lazy.t

val (!!) : 'a t -> 'a
(** Same as Lazy.force *)
(* CR jfuruse: (!!) cannot be put in the module Open :-( *)

val eager : 'a -> 'a t
val from_val : 'a -> 'a t

val peek : 'a t -> 'a option
val is_val : 'a t -> bool
val detuple : ('a * 'b) t -> 'a t * 'b t
