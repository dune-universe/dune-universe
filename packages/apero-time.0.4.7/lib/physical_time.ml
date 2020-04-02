open Time

module type Physical_time = sig
  include Time

  val of_seconds: float -> t
  val to_seconds: t -> float

end
