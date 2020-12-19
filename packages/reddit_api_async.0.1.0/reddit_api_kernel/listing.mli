open! Core_kernel

module Page_id : sig
  type t [@@deriving sexp]

  include Stringable with type t := t
end

module Pagination : sig
  type t =
    | Before of Page_id.t
    | After of Page_id.t
  [@@deriving sexp]
end

type +'child t [@@deriving sexp]

val of_json : (Json.t -> 'child) -> Json.t -> 'child t
val children : 'child t -> 'child list
val after : _ t -> Page_id.t option
val map : 'a t -> f:('a -> 'b) -> 'b t
