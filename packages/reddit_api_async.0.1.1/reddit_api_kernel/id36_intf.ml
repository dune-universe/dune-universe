open! Core_kernel

module type S = sig
  type t [@@deriving sexp]

  include Identifiable.S with type t := t

  val of_int63 : Int63.t -> t
  val to_int63 : t -> Int63.t
end

module type Id36 = sig
  module type S = S

  include S
end
