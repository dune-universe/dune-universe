type t

(** [to_int_list] converts a bitstring to a list of octet values *)
val to_int_list : t -> int list

(** [to_hex_string] converts a bitstring into a hex string with octet values
    separated by [sep]. [sep] defaults to space. *)
val to_hex_string : ?sep:string -> string -> string

(** [to_bit_list] converts a bitstring to a list of set bits. The left most
    bit is 0 *)
val to_bit_list : t -> int list

(** [to_string] converts a bitstring to a string in the format
    [Hex-Bytes BITS(Numbers of each set bit)]. eg [99 CC BITS(0 3 4 7 8 9 12 13)].
    The left most bit is bit 0.  *)
val to_string : t -> string
