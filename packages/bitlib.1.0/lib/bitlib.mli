val to_bits : int -> int list
(** [to_bits i]: Converts an integer, [i], to a list of bits.

  For example, [to_bits 5 => [1;0;1]] *)

val pad : int -> int list -> int list
(** [pad padding bits]: Ensures bits is of length [padding], left-padding the
    list with 0's as needed.

    For example, [padding 8 [1;0;1] => [0;0;0;0;0;1;0;1]] *)

val to_bytes: int list -> int -> int list
(** [to_bytes bits n]: Writes the bit sequence, [bits], as a byte
  sequence of [n] length. Assumes bytes are 8 bits.

    For example, [to_bytes (padding 32 (to_bits 256)) 4 => [0;0;1;0]]
    or [to_bytes [0;0;0;0;0;1;0;1] 1 => [5]]
*)

val little_endian_of_int : int -> int -> int list
(** [little_endian_of_int i bytes]: converts an integer, [i], to a list of
  bytes in little endian order.

  For example, [little_endian_of_int 5 2 => [0;5]] *)

val big_endian_of_int : int -> int -> int list
(** [big_endian_of_int i bytes]: converts an integer, [i], to a list of
  bytes in big endian order.

  For example, [big_endian_of_int 5 2 => [5;0]] *)

val write_binary_file : int list -> string -> unit
(** [write_binary_file bytes filename]: Creates a binary file named [filename]
  and writes the content of [bytes] to the file. *)
