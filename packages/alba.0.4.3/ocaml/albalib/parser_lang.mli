open Fmlib
open Module_types

open Ast

type 'a located = 'a Character_parser.Located.t

type indent   = Character_parser.Indent.t
type position = Character_parser.Position.t
type range    = Character_parser.Position.t * Character_parser.Position.t





module Problem:
sig
    type t =
    | Operator_precedence of
        string * string (* the 2 operatos strings *)

    | Illegal_name of string (* expectation *)

    | Illegal_command of string list

    | Ambiguous_command of string list

    | Duplicate_argument

    | Unused_definition of string

    | No_result_type

    | No_argument_type
end




module Command:
sig
    type t =
        | Evaluate of Expression.t
        | Type_check of Expression.t
        | Define of Expression.definition
        | Clear
        | Load of string located
        | Reload
        | Exit
        | Do_nothing
end



module Source_file:
sig
    type entry =
        | Expression of (bool * Expression.t)
        | Entry of Source_entry.t


    type t

    val count: t -> int

    val top: t -> entry
end



module type ERROR =
            Generic_parser.ERROR
                with type expect   = string * Character_parser.Indent.t
                and  type semantic = range * Problem.t



module type SIG =
    sig
        type parser
        type state
        type final
        type _ t

        module Error: ERROR

        val needs_more: parser -> bool
        val has_ended:  parser -> bool
        val has_succeeded: parser -> bool
        val has_failed: parser -> bool
        val state: parser -> Source_file.t

        val put_character: parser -> char -> parser
        val put_end:  parser -> parser

        val result: parser -> final option
        val error:  parser -> Error.t
        val line: parser -> int
        val column: parser -> int
        val position: parser -> position
        val error_tabs: parser -> int list

        val expression: unit -> Expression.t t
        val command: Command.t t
        val global_definition:  _ -> Expression.definition t
        val global_definitions: _ -> Expression.definition array t
        val inductive_type:     _ -> Source_entry.inductive t
        val inductive_family:   _ -> Source_entry.inductive array t
        val source_file: bool -> unit t
        val make: final t -> parser
        val run: final t -> string -> parser

        module Error_printer (PP: Pretty_printer.SIG):
        sig
            val print_with_source: string -> parser -> PP.t
            val print_with_source_lines:
                string Sequence.t -> parser -> PP.t
        end
    end


module Make (Final: ANY):
sig
    include SIG with type final = Final.t
                and type state = Source_file.t
end
