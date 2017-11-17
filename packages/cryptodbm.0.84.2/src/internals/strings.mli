
(* Append/remove some padding. *)

(* padding length must be >= 0. *)
val pad : string -> int -> string

(* string length must be >= 2. *)
val unpad : string -> string

(* Append/read a char. *)
val append_char   : string -> char -> string

(* string length must be >= 1. *)
val get_last_char : string -> (char * string)
val get_first_char : string -> (char * string)

(* Inserts/read a 16-bit value in string s, at position p. Big endian.
 * p must be between 0 and length s - 2 *)
val insert16 : string -> pos:int -> int -> string
val read16   : string -> pos:int -> int

