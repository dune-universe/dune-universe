type t = A | B

val x : t

module type S = sig
  type t

  val perform : t -> unit
end

module Make (X : S) : sig
  type t = X.t

  val perform_twice : t -> unit
end
