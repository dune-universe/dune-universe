(** Get the color code (24 bits) of the given name. [None] if it does not
    exists.
*)
val color_of_string: string -> Ocolor_types.rgb option

(** Get the name of a color code (24 bits) [None] if not found.
*)
val string_of_color: Ocolor_types.rgb -> string option

(** Set of avaiable colors name, without prefix, or anything.
*)
val available_colors : (string * Ocolor_types.rgb) list
