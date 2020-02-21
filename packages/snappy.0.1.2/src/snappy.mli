(** Snappy compression/uncompression *)

(** This exception is raised on malformed input *)
exception Error of string

(** {2 Easy interface} *)

(** @return compressed string *)
val compress : string -> string

(** @return whether input string can be successfully uncompressed *)
val is_valid : string -> bool

(** @return number of bytes needed to store the result of uncompressing given input *)
val get_uncompressed_size : string -> int

(** @return uncompressed string *)
val uncompress : string -> string

(** {2 Substring interface}

[function_sub input offset length] is equivalent to [function] but operates on the substring of the given
[input] string, defined by [offset] and [length], raising [Invalid_argument] when [offset] and [length] do not
represent a valid substring of [input] *)

val compress_sub : string -> int -> int -> string
val is_valid_sub : string -> int -> int -> bool
val get_uncompressed_size_sub : string -> int -> int -> int
val uncompress_sub : string -> int -> int -> string

(** {2 Unsafe interface}

Substring interface without bounds checking *)

val unsafe_compress : string -> int -> int -> string
val unsafe_is_valid : string -> int -> int -> bool
val unsafe_get_uncompressed_size : string -> int -> int -> int
val unsafe_uncompress : string -> int -> int -> string
