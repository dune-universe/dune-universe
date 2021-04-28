module type Metric = sig
  type t

  (** This should be a proper distance function (symmetric, zero on the diagonal,
      verifying the triangular inequality). *)
  val dist : t -> t -> float
end
