(** Poorman's hashset by Hashtbl *)

(* CR jfuruse: It is mainly for hash consing but it wastes memory... Self contradiction. *)
type 'a t
val create : ?random:bool -> int -> 'a t
val add : 'a t -> 'a -> unit
val remove : 'a t -> 'a -> unit
val mem : 'a t -> 'a -> bool

val find : 'a t -> 'a -> 'a
val find_opt : 'a t -> 'a -> 'a option 
(** [find] and [find_opt] finds the 'same' element in the set. Good for hash consing *)

val find_or_add : 'a t -> 'a -> 'a

val iter : ('a -> unit) -> 'a t -> unit
val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
val elements : 'a t -> int
val clear : 'a t -> unit
val of_list : int -> 'a list -> 'a t
val to_list : 'a t -> 'a list


module Make(A : Hashtbl.HashedType) : sig
  type t
  val create : int -> t
  val add : t -> A.t -> unit
  val remove : t -> A.t -> unit
  val mem : t -> A.t -> bool
  val find : t -> A.t -> A.t
  val find_opt : t -> A.t -> A.t option
  val find_or_add : t -> A.t -> A.t
  val iter : (A.t -> unit) -> t -> unit
  val fold : (A.t -> 'a -> 'a) -> t -> 'a -> 'a
  val elements : t -> int
  val clear : t -> unit
  val of_list : int -> A.t list -> t
  val to_list : t -> A.t list
end
