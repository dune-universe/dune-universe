open Fmlib
open Alba_core


type pos = Character_parser.Position.t
type range = pos * pos


type type_in_context = Build_context.type_in_context

type description =
    | Overflow
    | No_name
    | Incomplete_type of type_in_context
    | Cannot_infer_bound
    | Not_a_function of type_in_context list
    | Wrong_type of (type_in_context * type_in_context) list
    | Wrong_base of type_in_context list * type_in_context list
    | Ambiguous of type_in_context list
    | Name_violation of string * string (* case, kind *)
    | Ambiguous_definition
    | Wrong_parameter_count of int
    | Wrong_parameter_name of string
    | Wrong_parameter_type of Term.typ * Gamma.t
    | Missing_inductive_type
    | No_inductive_type
    | Duplicate_inductive
    | Duplicate_constructor
    | Wrong_type_constructed of Term.typ * Gamma.t
    | Negative
    | Nested_negative of Inductive.t * int * Gamma.t
    | Not_positive of Term.typ * Gamma.t
    | Not_yet_implemented of string


type t = range * description





module Print (P: Pretty_printer.SIG):
sig
    val description: description -> P.t

    val print_with_source: string -> t -> P.t

    val print_with_source_lines:
        string Sequence.t -> t -> P.t
end


val string_of_problem: string -> t -> string
