open Stdint

type required_len_and_seek_to = { required_len : int64
                                ; seek_to      : int64
                                }

exception Invalid_range

(*val pad_bytes : ?filler:uint8 -> bytes -> int -> bytes *)

val get_sub_string : string -> pos:int -> len:int -> string

val get_sub_string_inc_range : string -> start_at:int -> end_at:int     -> string

val get_sub_string_exc_range : string -> start_at:int -> end_before:int -> string

val list_find_option : ('a -> bool) -> 'a list -> 'a option

val char_list_to_string : char list -> string

val make_path : string list -> string

val path_to_list : string -> string list

val path_to_file : string -> string

val get_option_ref_init_if_none : (unit -> 'a) -> 'a option ref -> 'a

val pad_string : ?filler:char -> string -> int -> string

val round_down_to_multiple_int64 : multiple_of:int64 -> int64 -> int64

val round_down_to_multiple       : multiple_of:int   -> int   -> int

val round_up_to_multiple_int64   : multiple_of:int64 -> int64 -> int64

val round_up_to_multiple         : multiple_of:int   -> int   -> int

val ensure_at_least              : at_least:'a       -> 'a    -> 'a

val ensure_at_most               : at_most:'a        -> 'a    -> 'a

val calc_required_len_and_seek_to_from_byte_range :
  from_byte:int64 option ->
  to_byte:int64 option ->
  force_misalign:bool ->
  bytes_so_far:int64 ->
  last_possible_pos:int64 ->
  required_len_and_seek_to
