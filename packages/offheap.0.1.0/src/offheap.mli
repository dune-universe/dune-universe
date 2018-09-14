(** Copies objects out of the OCaml heap where they are not managed by the GC *)

type 'a t

val copy : 'a -> 'a t
  (** [create obj] creates a deep copy of an object outside the OCaml heap.
    *
    * The object cannot point to any abstract or custom objects and the copy
    * cannot be mutated to point to in-heap objects later on.
    *
    * @raise [Invalid_argument] if the object cannot be moved off-heap.
    *)

val get : 'a t -> 'a
  (** [get ref] returns a refrence to the off-heap object.
    *
    * @raise [Invalid_argument] if the object was already deleted.
    *)

val free : 'a t -> unit
  (** [free obj] deletes the off-heap object [obj].
    *
    * The object should not be accessed afterwards through other references.
    *
    * @raise [Invalid_argument] if the object was already deleted.
    *)
