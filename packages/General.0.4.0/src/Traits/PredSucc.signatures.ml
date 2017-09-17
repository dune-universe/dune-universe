module type S0 = sig
  type t

  val succ: t -> t
  val pred: t -> t
end
