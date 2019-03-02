open Parsexp

val load
  :  (module Parser with type parsed_value = 'a)
  -> filename:string
  -> ('a, Parse_error.t) result
val load_exn
  :  (module Parser with type parsed_value = 'a)
  -> filename:string
  -> 'a

type ('a, 'b) conv_mode =
  | Single : ('a, 'a     ) conv_mode
  | Many   : ('a, 'a list) conv_mode

val load_conv
  :  ('a, 'b) conv_mode
  -> filename:string
  -> (Base.Sexp.t -> 'a)
  -> ('b, Conv_error.t) result

val load_conv_exn
  :  ('a, 'b) conv_mode
  -> filename:string
  -> (Base.Sexp.t -> 'a)
  -> 'b
