(*i $Id: polka.mli,v 1.4 2004/02/27 10:34:10 bjeannet Exp $ i*)

exception Overflow of string

type dimsup = {
  pos: int;
  nbdims: int;
}

val strict : bool ref
val dec : int ref
val print_limit : int ref

val initialize : strict:bool -> maxdims:int -> maxrows:int -> unit
val finalize :  unit -> unit
external set_gc : int -> unit = "camlidl_polka_set_gc"
external set_widening_affine : unit -> unit = "camlidl_polka_set_widening_affine"
external set_widening_linear : unit -> unit = "camlidl_polka_set_widening_linear"

val denominator_of_list : (Big_int.big_int * Big_int.big_int * string) list -> Big_int.big_int

val to_constraint : (int -> string) -> (int -> Big_int.big_int) -> int -> string
val to_frame : (int -> string) -> (int -> Big_int.big_int) -> int -> string
val to_expr : (int -> string) -> (int -> Big_int.big_int) -> int -> string


val print_list :
  Format.formatter ->
  (unit,Format.formatter,unit) format ->
  (unit,Format.formatter,unit) format ->
  (unit,Format.formatter,unit) format ->
  (Format.formatter -> 'a -> unit) -> 'a list -> unit

type cons = Egal | SupEgal | Sup
type gen = Vertex | Ray | Line
