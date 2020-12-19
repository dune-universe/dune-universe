open! Core_kernel

module Conversation : sig
  type t [@@deriving sexp_of]

  include Json_object.S_with_fields with type t := t
end
