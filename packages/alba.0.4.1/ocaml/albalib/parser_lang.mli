open Fmlib
open Module_types

open Ast

type 'a located = 'a Character_parser.Located.t

type position = Character_parser.Position.t
type range = Character_parser.Position.t * Character_parser.Position.t





module Problem:
sig
  type t =
    | Operator_precedence of
        range
        * string * string (* the 2 operatos strings *)

    | Illegal_name of range * string (* expectation *)

    | Illegal_command of range * string list

    | Ambiguous_command of range * string list

    | Duplicate_argument of range
end




module Command:
sig
  type t =
    | Evaluate of Expression.t
    | Type_check of Expression.t
    | Exit
    | Do_nothing
end




module type SIG =
    sig
        type parser
        type final
        type _ t

        module Error: Generic_parser.ERROR with type expect = string
                                            and type semantic = Problem.t

        val needs_more: parser -> bool
        val has_ended:  parser -> bool
        val has_succeeded: parser -> bool

        val put_char: parser -> char -> parser
        val put_end:  parser -> parser

        val result: parser -> final option
        val error:  parser -> Error.t
        val line: parser -> int
        val column: parser -> int
        val position: parser -> position

        val expression: unit -> Expression.t t
        val command: Command.t t
        val make: final t -> parser
        val run: final t -> string -> parser
    end


module Make (Final: ANY): SIG with type final = Final.t
