(** This modules implements a cheap term parser to handle infix
    operators associativity and precedence without changing the menhir
    grammar files *)

open Logic.Abstract_syntax
open AcgData.Environment

(** The type of the tokens. Only [Op] values can have infix syntactic properties *)
type token =
  | Term of (Abstract_syntax.term * Abstract_syntax.location)
  | Op of ( Abstract_syntax.term * Abstract_syntax.syntactic_behavior * Abstract_syntax.location)


(** [parse_sequence lst sg] returns a pairs consisting of a term and
    its location. The sequence is parsed according to the
    associativity and precedence properties given in [sg]. *)
val parse_sequence : token list -> Environment.Signature1.t -> Abstract_syntax.term*Abstract_syntax.location
