external format_float: string -> float -> string
  = "caml_format_float"
external format_int: string -> int -> string
  = "caml_format_int"
external format_int32: string -> int32 -> string
  = "caml_int32_format"
external format_nativeint: string -> nativeint -> string
  = "caml_nativeint_format"
external format_int64: string -> int64 -> string
  = "caml_int64_format"

val format_camlfloat: string -> float -> string

type 'a out = {
  string : string -> unit;
  char : char -> unit;
  flush : unit -> unit;
  finish : unit -> 'a;
}

type ('ret, 'b) t = 'b out -> 'ret
(** The sprintf formatter type *)

val sprintf : ('ret, string) t -> 'ret
