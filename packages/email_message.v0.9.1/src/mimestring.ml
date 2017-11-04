open Core

module type S = sig
  type t = private string [@@deriving sexp, bin_io, hash]
  val of_string : string -> t
  val to_lowercase_string : t -> string
  val equal_string : t -> string -> bool

  include Comparable.S_binable with type t := t
  include Hashable.S_binable with type t := t
end

(** Case-insensitive strings *)
module Case_insensitive = struct
  include String.Caseless

  let of_string = Fn.id
  let to_lowercase_string = String.lowercase
  let equal_string = equal
end

let quote_escape =
  unstage (String.Escaping.escape ~escapeworthy:['"'; '\\'] ~escape_char:'\\')
;;

let quote str = String.concat ["\""; quote_escape str; "\""];;
