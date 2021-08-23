include Intf_std
include Intf_algebra
include Intf_monad
include Intf_infix
module Lang = Intf_lang

(** Metric space *)
module type Metric = sig
  include Std

  (** [dist] is expected to be a proper distance function (symmetric, zero on the diagonal,
      verifying the triangular inequality). *)
  val dist : t -> t -> float
end

(** Module type of anything that looks like the (measured) reals. *)
module type Reals = sig
  include Field_std

  (** Lebesgue on the interval [0;1] *)
  val lebesgue : Random.State.t -> t

  val ( + ) : t -> t -> t

  val ( - ) : t -> t -> t

  val ( * ) : t -> t -> t

  val ( / ) : t -> t -> t

  include Infix_order with type t := t and type 'a m := 'a

  (** [npow x n] is [x] tp the power of [n]. *)
  val npow : t -> int -> t

  val to_float : t -> float

  val of_float : float -> t

  val of_int : int -> t
end
