
(* Extensible bit arrays *)

type bitarray

type t = bitarray

(* Initially, all bits are set to false. 
 * The argument is an indicative starting size (in bits). *)
val create : int -> bitarray

val get : bitarray -> int -> bool
val set : bitarray -> int -> bool -> unit

(* How many bits are set *)
val count : bitarray -> int

(* Fold only on bits which are set. *)
val fold : bitarray -> 'a -> (int -> 'a -> 'a) -> 'a

(* Iter only on bits which are set. *)
val iter : bitarray -> (int -> unit) -> unit
