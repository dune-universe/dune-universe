(** [Text] is a series of Unicode code points.

    The type is abstract so that the compiler will remind us not to use [String.length]
    when we mean [Text.width].
*)

open! Core_kernel
open! Import

type t [@@deriving compare, quickcheck, sexp_of]

include Container.S0 with type t := t with type elt := Uchar.t
include Stringable.S with type t := t

(** [width t] approximates the displayed width of [t].

    We incorrectly assume that every code point has the same width. This is better than
    [String.length] for many code points, but doesn't work for double-width characters or
    combining diacritics. *)
val width : t -> int

(** [bytes t] is the number of bytes in the UTF-8 encoding of [t]. *)
val bytes : t -> int

(** [chunks_of t ~width] splits [t] into chunks no wider than [width] s.t. {[

      t = t |> chunks_of ~width |> concat

    ]}.  [chunks_of] always returns at least one chunk, which may be empty. *)
val chunks_of : t -> width:int -> t list

val of_uchar_list : Uchar.t list -> t
val concat : t list -> t

(** [iteri t ~f] calls [f index uchar] for every [uchar] in [t]. [index] counts
    characters, not bytes. *)
val iteri : t -> f:(int -> Uchar.t -> unit) -> unit

(** [split t ~on] returns the substrings between and not including occurrences of [on]. *)
val split : t -> on:char -> t list
