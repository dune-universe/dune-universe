type t
type column =
  | Ints of int array
  | Int_opts of int option array
  | Floats of float array
  | Float_opts of float option array
  | Strings of string array
  | String_opts of string option array

val nrows : t -> int

val ncols : t -> int

val get_col : t -> int -> column option

val get_col_by_name : t -> string -> column option

val columns : t -> (string * column) list

val make : (string * column) list -> (t, [> `Msg of string]) result

val from_file :
  ?header:[`Read_in_file | `Expect of string list | `Use of string list | `None] ->
  string ->
  (t, [> `Msg of string]) result

exception Error of string

module Ez : sig
  val from_file :
    ?header:[`Read_in_file | `Expect of string list | `Use of string list | `None] ->
    string ->
    t
  val get_ints : t -> int -> int array
  val get_int_opts : t -> int -> int option array
  val get_floats : t -> int -> float array
  val get_float_opts : t -> int -> float option array
  val get_strings : t -> int -> string array
  val get_string_opts : t -> int -> string option array

  val get_ints_by_name : t -> string -> int array
  val get_int_opts_by_name : t -> string -> int option array
  val get_floats_by_name : t -> string -> float array
  val get_float_opts_by_name : t -> string -> float option array
  val get_strings_by_name : t -> string -> string array
  val get_string_opts_by_name : t -> string -> string option array
end

type html_formatter =
  int -> string -> Html_types.td_content Tyxml.Html.elt

val to_html :
  ?formatters:(string * html_formatter) list ->
  t ->
  [> Html_types.table ] Tyxml.Html.elt
