open Format

type t = formatter

val stdout : t
val stderr : t

val list : (unit, t, unit) format -> (t -> 'a -> unit) -> t -> 'a list -> unit

val option : (t -> 'a -> unit) -> t -> 'a option -> unit
val lazy_ : (t -> 'a -> unit) -> t -> 'a Lazy.t -> unit

val hbox   : t -> unit -> unit
val vbox   : t -> int -> unit
val hvbox  : t -> int -> unit
val hovbox : t -> int -> unit
val box    : t -> int -> unit
val tag    : t -> string -> unit
val tbox   : t -> unit -> unit

val close_box  : t -> unit -> unit
val close_tag  : t -> unit -> unit
val close_tbox : t -> unit -> unit

val string        : t -> string -> unit
val as_           : t -> int -> string -> unit
val int           : t -> int -> unit
val float         : t -> float -> unit
val char          : t -> char -> unit
val bool          : t -> bool -> unit
val break         : t -> int -> int -> unit
val tbreak        : t -> int -> int -> unit
val cut           : t -> unit
val space         : t -> unit
val force_newline : t -> unit
val flush         : t -> unit
val newline       : t -> unit
val if_newline    : t -> unit
val tab           : t -> unit

val set_tab : t -> unit -> unit
val set_tags : t -> bool -> unit
val set_print_tags : t -> bool -> unit
val set_mark_tags : t -> bool -> unit
val print_tags : t -> unit -> bool
val mark_tags : t -> unit -> bool
val set_margin : t -> int -> unit
val margin : t -> unit -> int
val set_max_indent : t -> int -> unit
val max_indent : t -> unit -> int
val set_max_boxes : t -> int -> unit
val max_boxes : t -> unit -> int
val over_max_boxes : t -> unit -> bool
val set_ellipsis_text : t -> string -> unit
val ellipsis_text : t -> unit -> string
val set_formatter_out_channel : t -> out_channel -> unit
val set_formatter_output_functions : t -> (string -> int -> int -> unit) -> (unit -> unit) -> unit
val formatter_output_functions : t -> unit -> (string -> int -> int -> unit) * (unit -> unit)
val set_formatter_out_functions : t -> formatter_out_functions -> unit
val formatter_out_functions : t -> unit -> formatter_out_functions
val set_formatter_tag_functions : t -> formatter_tag_functions -> unit
val formatter_tag_functions : t -> unit -> formatter_tag_functions

val of_out_channel : out_channel -> t

val to_string : (Format.formatter -> 'a -> unit) -> 'a -> string

val sprintf : ('a, t, unit, string) format4 -> 'a
  (** [sprintf] with a better type than the orignal *)

val ksprintf : (string -> 'a) -> ('b, t, unit, 'a) format4 -> 'b
