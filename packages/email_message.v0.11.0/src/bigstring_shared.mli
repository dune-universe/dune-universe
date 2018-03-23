open! Core

(** Immutable sequences of bytes which can be windowed efficiently. *)
type t = private Bigstring.t [@@deriving sexp_of, compare, hash]

val of_bigstring : Bigstring.t -> t
val to_bigstring : t -> Bigstring.t

include Stringable.S with type t := t
include String_monoidable.S with type t := t

val to_lexbuf : t -> Lexing.lexbuf

(** Empty, immutable Bigstring *)
val empty : t

val length : t -> int
val sub : ?pos:int -> ?len:int -> t -> t

val foldi : t -> init:'b -> f:(int -> 'b -> char -> 'b) -> 'b

(** [include_empty_last_line] determines whether a string that ends in "\n" has an empty
    string as the last line.

    [iter_lines] and [split_lines] do not include an empty last line. *)
val lines_seq : ?include_empty_last_line:unit -> t -> t Sequence.t

val iter_lines : t -> f:(t -> unit) -> unit
val split_lines : t -> t list

(** Gets a bigstring from a bigbuffer with minimal memory overhead. *)
val of_bigbuffer_volatile : Bigbuffer.t -> t

val of_string_monoid : String_monoid.t -> t

val substr_index : ?pos:int -> t -> pattern:t -> int option

module Stable : sig
  module V1 : sig type nonrec t = t [@@deriving sexp, bin_io] end
end
