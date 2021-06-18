type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type error = Msg of string | Partial

val parse_headers :
  ?pos:int -> ?len:int -> bigstring -> (Cohttp.Header.t * int, error) result

val parse_request :
  ?pos:int -> ?len:int -> bigstring -> (Cohttp.Request.t * int, error) result
(** Attempts to parse a buffer into a HTTP request. If successful, it returns
    the parsed request and an offset value that indicates the starting point of
    unconsumed content left in the buffer. *)

val parse_chunk_length :
  ?pos:int -> ?len:int -> bigstring -> (int64 * int, error) result

val parse_chunk :
  ?pos:int -> ?len:int -> bigstring -> (string option * int, error) result
