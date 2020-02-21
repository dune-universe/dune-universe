
exception Error of string

external unsafe_compress : string -> int -> int -> string = "caml_snappy_compress"
external unsafe_is_valid : string -> int -> int -> bool = "caml_snappy_is_valid"
external unsafe_get_uncompressed_size : string -> int -> int -> int = "caml_snappy_get_u_size"
external unsafe_uncompress : string -> int -> int -> string = "caml_snappy_uncompress"

let validate s ofs len name =
  if ofs < 0 || len < 0 || ofs > String.length s - len then invalid_arg name

let compress_sub s ofs len =
  validate s ofs len "Snappy.compress_sub";
  unsafe_compress s ofs len

let is_valid_sub s ofs len =
  validate s ofs len "Snappy.is_valid_sub";
  unsafe_is_valid s ofs len

let get_uncompressed_size_sub s ofs len =
  validate s ofs len "Snappy.get_uncompressed_size_sub";
  unsafe_get_uncompressed_size s ofs len

let uncompress_sub s ofs len =
  validate s ofs len "Snappy.uncompress_sub";
  unsafe_uncompress s ofs len

let compress s = unsafe_compress s 0 (String.length s)
let is_valid s = unsafe_is_valid s 0 (String.length s)
let get_uncompressed_size s = unsafe_get_uncompressed_size s 0 (String.length s)
let uncompress s = unsafe_uncompress s 0 (String.length s)

let () =
  Callback.register_exception "Snappy.Error" (Error "")

