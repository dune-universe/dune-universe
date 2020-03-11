open Fmlib


type pos = Character_parser.Position.t
type range = pos * pos


type problem_description

type problem = range * problem_description

val build:
    Ast.Expression.t
    -> Context.t
    -> ((Term.t * Term.typ) list, problem) result


module Print (P: Pretty_printer.SIG):
sig
    val description: problem_description -> P.t
end
