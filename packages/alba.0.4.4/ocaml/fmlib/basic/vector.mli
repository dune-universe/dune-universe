(** A mutable vector (like a C++ vector) *)

type 'a t

(** Make an empty vector *)
val make_empty: unit -> 'a t

(** A singleton vector *)
val singleton: 'a -> 'a t

(** [make n e] makes a vector with size [n] where all elements are [e]. *)
val make:  int -> 'a -> 'a t

(** Number of elements in the vector. *)
val count: 'a t -> int

(** Is the vector empty? *)
val is_empty: 'a t -> bool

(** [elem v i] returns the [i]th element of the vector [v]. *)
val elem:  'a t -> int -> 'a

(** The first element of the vector. *)
val first: 'a t -> 'a

(** The last element of the vector. *)
val last:  'a t -> 'a

(** [copy v] returns a copy of the vector [v]. *)
val copy:  'a t -> 'a t

(** [put v i e] sets the [i]th element of the vector [v] to [e]. *)
val put:   'a t -> int -> 'a -> unit

(** [push_rear v e] appends the element [e] to the end of the vector [v]. *)
val push_rear:  'a t -> 'a -> unit

(** [remove_n_rear v n] removes the last [n] elements of the vector [v]. *)
val remove_n_last:   'a t -> int -> unit

(** [remove_last v] removes the last element of the vector [v]. *)
val remove_last: 'a t -> 'a

(** [keep v n] keeps only the first [n] elements of [v]. *)
val keep:  'a t -> int -> unit


(** [remove v i] removes the [i]th element from the vector [v]. *)
val remove: 'a t -> int -> unit
