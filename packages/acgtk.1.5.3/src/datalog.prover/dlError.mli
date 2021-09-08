type err =
     | BadArity of (string * int * int)
     | ParseError of string

module E : UtilsLib.ErrorMg.E with type t = err

module Error : UtilsLib.ErrorMg.ERROR with type synt_error = E.t
