(** Retrieve the X Logical Font Description ("XLFD") full name for every font
   available on the platform at hand. *)

exception Invalid_platform of string;;
exception Invalid_XLFD of string;;

val all : unit -> (Xfonts.font_name * Xfonts.x_font) list;;
