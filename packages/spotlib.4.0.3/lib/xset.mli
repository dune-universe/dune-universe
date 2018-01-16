module type S = sig
  include Set.S
  val of_list : elt list -> t

  val (+) : t -> t -> t
  (** union *)

  val (-) : t -> t -> t
  (** diff *)

  val one : elt -> t
  (** singleton *)
    
  val unions : t list -> t

  val to_list : t -> elt list
  (** elements *)

  val unsafe_binary : t -> (t * elt * t) option

  val unsafe_middle : t -> elt option
  (** find the middle element in [t] *)

  val unsafe_find : elt -> t -> elt option
  (** find the "same" element as [elt] in [t] *)

end

module Make(O : Set.OrderedType) : S 
  with type elt = O.t
