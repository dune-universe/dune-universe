(** Bijections between finite sets and [{0; ...; n-1}]. *)

module type S = sig
  (** The type of bijections. *)
  type t

  (** The type of elements in bijection with [{0; ..; n-1}]. *)
  type elt

  (** [of_list l] Constructs a bijection between l and [{0; ...; n-1}].
      The bijection is constructed by assigning to the elements l{_0}, l{_1}, ..., l{_n-1} the
      indices [{0; 1; ...; n-1}].  *)
  val of_list : elt list -> t

  (** Get the nth element out of the bijection. This is O(1).

      @raise Invalid_argument if the element is  out of the range of the bijection.  *)
  val nth_exn : t -> int -> elt

  (** Same as [nth_exn] but returns None if out of range. *)
  val nth_opt : t -> int -> elt option

  (** Returns the index of a given element. This is O(log2(n)) for a
      set with [n] elements when using the map-based implementation.

      @raise Not_found if element is not in the domain of the bijection. *)
  val idx_exn : t -> elt -> int

  (** See [idx_opt] *)
  val idx_opt : t -> elt -> int option

  (** Returns the size of domain of the bijection. *)
  val support : t -> int

  (** Folds over the graph of the bijection. *)
  val fold : (elt -> int -> 'a -> 'a) -> t -> 'a -> 'a
end

(** Map-based implementation. *)
module Make : functor (Elt : Basic_intf.Ordered) -> S with type elt = Elt.t
