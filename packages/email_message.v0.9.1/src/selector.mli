open! Core
open Re2

module Base : sig
  (* The "exists" query can equally be accomplished using [sexp query], but
     not the "all" one. *)
  type t =
    [ `exists_header of string * Regex.t
    | `all_headers   of string * Regex.t
    ] [@@deriving sexp]

  val matches : t -> Email.t -> bool

  val examples : t list
end

type t = Base.t Blang.t [@@deriving sexp]

val matches : t -> Email.t -> bool

val example : t
