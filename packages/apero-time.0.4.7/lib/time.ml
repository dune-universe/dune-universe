module type Time = sig
  include Apero.Ordered.S

  val after: t -> t -> bool
  val before: t -> t -> bool

  val to_string: t -> string
  val of_string: string -> t option

  val to_rfc3339: t -> string
  val to_seconds: t -> float

  val encode: t -> Abuf.t -> unit
  val decode: Abuf.t -> t

  val pp: Format.formatter -> t -> unit
end
