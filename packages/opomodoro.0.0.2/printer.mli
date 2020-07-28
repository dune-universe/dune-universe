type pixel
type icon

(** Return the icon for a given digit as char **)
val get_char_icon : char -> icon

(** Given an icon and index, return pixel at that index **)
val get_pixel : icon -> int -> pixel

(** Given a pixel, get its ANSI styles for display **)
val get_pixel_style : pixel -> ANSITerminal.style list

(** Given a x y position, print the pixel to the screen **)
val print_pixel : int -> int -> pixel -> unit

(** Given a start x and y, print out the char to position on screen **)
val print_char : int -> int -> char -> unit

(** Return the string represention of the time in seconds **)
val to_timestring : int -> string

(** Given the time in seconds, display time in terminal **)
val display_time : int -> unit
