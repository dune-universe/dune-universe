module Make0(M: sig
  type t

  val negate: t -> t
  val add: t -> t -> t
  val substract: t -> t -> t
  val multiply: t -> t -> t
  val divide: t -> t -> t
  val exponentiate: t -> int -> t
end): sig
  val (~+): M.t -> M.t
  val (~-): M.t -> M.t
  val (+): M.t -> M.t -> M.t
  val (-): M.t -> M.t -> M.t
  val ( * ): M.t -> M.t -> M.t
  val (/): M.t -> M.t -> M.t
  val ( ** ): M.t -> int -> M.t
end
