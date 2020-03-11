open Fmlib

module Located = Character_parser.Located

module Position = Character_parser.Position

type range = Position.t * Position.t


module Expression:
sig
  type operator = string * Operator.t

  type argument_type =
    | Normal
    | Operand


  type t =
    t0 Located.t

  and t0 =
    | Proposition
    | Any
    | Identifier of string
    | Number of string
    | Char of int
    | String of string
    | Operator of operator
    | Typed of t * t
    | Application of t * (t * argument_type) list
    | Function of
        formal_argument list
        * t option                        (* result type *)
        * t                               (* defining expression *)
    | Product of formal_argument list * t

  and formal_argument =
    string Located.t * t option


    val binary: t
                -> (operator Located.t * t) list
                -> (t, range * string * string) result
end
