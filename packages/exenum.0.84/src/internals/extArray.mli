
(* Extensible array *)
type 'a t

(* Starting size, default value. *)
val create : int -> 'a -> 'a t

(* Grow the array if necessary, filling new cells with the default value. *)
val set : 'a t -> int -> 'a -> unit

(* If a cell has never been set, it contains the default value. 
 * May raise Failure if index is < 0. *)
val get : 'a t -> int -> 'a

val size : 'a t -> int

val fold : 'a t -> 'b -> (int -> 'a -> 'b -> 'b) -> 'b

(* Called in increasing order. *)
val iter : 'a t -> (int -> 'a -> unit) -> unit

val copy : 'a t -> 'a t

