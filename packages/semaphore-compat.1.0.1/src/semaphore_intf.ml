module type COUNTING = sig
  type t

  val make : int -> t
  val release : t -> unit
  val acquire : t -> unit
  val try_acquire : t -> bool
  val get_value : t -> int
end

module type BINARY = sig
  type t

  val make : bool -> t
  val release : t -> unit
  val acquire : t -> unit
  val try_acquire : t -> bool
end

module type S = sig
  module Counting : COUNTING
  module Binary : BINARY
end
