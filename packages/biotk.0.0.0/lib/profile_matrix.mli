open Biotk_croquis

module type S = sig
  type t = private float array array
  val of_array : float array array -> t option
  val flat : int -> t
  val length : t -> int
  val composition : t -> float array
  val draw : t -> Croquis.Picture.t
  val entropy : t -> float array
end

module type Alphabet = sig
  type t
  val all : t list
  val card : int
  val to_char : t -> char
  val of_char : char -> t option
  val of_char_exn : char -> t
  val to_int : t -> int
end

module Make(A : Alphabet) : S

module DNA : sig
  include S
  val reverse_complement : t -> t
end
