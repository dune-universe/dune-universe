open Core_kernel

(** two dimensional blocks of text *)
type t [@@deriving sexp_of]

include Invariant.S with type t := t

(** the empty block. a left and right unit to both [hcat] and [vcat] *)
val nil : t

(** [fill] and [space] assume width and height are non-negative *)
val fill : char -> width:int -> height:int -> t

val space : width:int -> height:int -> t

(** vertical and horizontal alignment specifications *)
type valign =
  [ `Top
  | `Bottom
  | `Center ]

type halign =
  [ `Left
  | `Right
  | `Center ]

(** a basic block of text, split on newlines and horizontally aligned as specified.

    If [max_width] is provided, split on whitespace and wrap words to respect the request.
    So long as no words are longer than [max_width], the resulting text block will be no
    wider than [max_width]
*)
val text : ?align:halign -> ?max_width:int -> string -> t

val textf : ?align:halign -> ?max_width:int -> ('r, unit, string, t) format4 -> 'r

(** vertical and horizontal concatenation with alignment *)
val vcat : ?align:halign -> ?sep:t -> t list -> t

val hcat : ?align:valign -> ?sep:t -> t list -> t

(** text block dimensions *)
val width : t -> int

val height : t -> int

(** vertical and horizontal sequence alignment *)
val valign : valign -> t list -> t list

val halign : halign -> t list -> t list

(** empty blocks with either horizontal or vertical extent -- useful for specifying a
    minimum width or height in conjunction with valign or halign, respectively *)
val hstrut : int -> t

val vstrut : int -> t

(** wrap a block with an ANSI escape sequence.
    The [prefix] and [suffix] arguments should render with zero width and height. *)
val ansi_escape : ?prefix:string -> ?suffix:string -> t -> t

(** render a block of text as a string *)
val render : t -> string

val table : ?sep_width:int -> [`Cols of (t list * halign) list] -> [`Rows of t list]

(** compress table header according to column widths.
    Input:  a list of columns of the form (title, values, column alignment).
    Output: one header block and row sequence.
    Raises: if the [values] lists are not the same length in each column.
    Example:

    {v
                                                 first name
      age  first name  last name            age  |     last name
      |    |           |            ==>     |    |     |
      30   sue         smith                30   sue   smith
      18   bill        rodriguez            18   bill  rodriguez
      76   rick        jones                76   rick  jones
    v} *)
val compress_table_header
  :  ?sep_width:int
  -> [`Cols of (t * t list * halign) list]
  -> [`Header of t] * [`Rows of t list]
(* convenience definitions *)

(** [vsep = vstrut 1] *)
val vsep : t

(** [hsep = hstrut 1] *)
val hsep : t

(** [indent ~n t = hcat [hstrut n; t]].  [n] defaults to [2] *)
val indent : ?n:int -> t -> t

(** [sexp sexp_of_a a = sexp_of_a a |> Sexp.to_string |> text] *)
val sexp : ('a -> Sexp.t) -> 'a -> t
