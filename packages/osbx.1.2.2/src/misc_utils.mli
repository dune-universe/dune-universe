open Stdint

exception Invalid_range

val pad_bytes : ?filler:uint8 -> bytes -> int -> bytes

val get_bytes : bytes -> pos:int -> len:int -> bytes

val get_bytes_inc_range : bytes -> start_at:int -> end_at:int     -> bytes

val get_bytes_exc_range : bytes -> start_at:int -> end_before:int -> bytes

val list_find_option : ('a -> bool) -> 'a list -> 'a option

val char_list_to_string : char list -> string

val make_path : string list -> string

val path_to_list : string -> string list

val path_to_file : string -> string

val get_option_ref_init_if_none : (unit -> 'a) -> 'a option ref -> 'a

val pad_string : string -> int -> char -> string
