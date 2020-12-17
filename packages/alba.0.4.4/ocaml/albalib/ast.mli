open Fmlib
open Alba_core

module Located = Character_parser.Located


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
        | Typed of t * t                      (* exp, type *)
        | Application of t * (t * argument_type) list
        | Function of
            formal_argument list
            * t option                        (* result type *)
            * t                               (* defining expression *)
        | Product of formal_argument list * t
        | Where of t * definition list
        | List of t list

    and formal_argument =
        string Located.t * t option

    and signature =
        formal_argument list * t option

    and named_signature =
        string Located.t * signature

    and definition =
        (string Located.t * formal_argument list * t option * t) Located.t


    type operand = operator Located.t list * t


    val to_list: t -> t0

    val find_unused_local: t -> definition list -> string Located.t option
end


module Operator_expression:
sig
    open Expression

    val make:
        operand
        -> (operator Located.t * operand) list
        -> (t, range * string * string) result
end







module Source_entry:
sig
    type named_signature =
        Expression.named_signature

    type inductive =
        named_signature * named_signature array

    type t =
        | Normal of Expression.definition
        | Inductive of inductive array
end
