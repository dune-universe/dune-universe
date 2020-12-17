(** An arrayed pool of elements of a certain type. *)


(** Type of the pool. *)
type 'a t



(** Make an empty pool. *)
val make_empty: unit -> 'a t



(** The current capacity of the pool. *)
val capacity: 'a t -> int



(** [has p i] Has the pool [p] an element at position [i]? *)
val has: 'a t -> int -> bool



(** [elem p i] The [i]th element of the pool [p]. *)
val elem: 'a t -> int -> 'a



(** [find p i] finds the next occupied element in the pool [p] starting from
   [i]. Returns [i] if [i] is occupied. Returns capacity if there is no next
   occupied element. *)
val find: 'a t -> int -> int


val iter: ('a -> unit) -> 'a t -> unit
(** [iter f p] Iterate over all occupied elements in the pool. *)



(** [occupy p a] occupies a place for the element [a] and puts it there. If
   there are no free elements, then allocate space in pool. Return the
   position of the element [a] in the pool. *)
val occupy: 'a t -> 'a -> int




(** [release p i] makes the position [i] in the pool free. *)
val release: 'a t -> int -> unit
