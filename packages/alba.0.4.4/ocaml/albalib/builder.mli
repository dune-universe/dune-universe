open Fmlib
open Alba_core
open Ast


type pos = Position.t
type range = pos * pos

(*
val build:
    Ast.Expression.t
    -> Context.t
    -> (Term.t * Term.typ, Build_problem.t) result



val build_definition:
    Ast.Expression.definition
    -> Context.t
    -> (Term.t * Term.typ, Build_problem.t) result
*)


val add_definition:
    Ast.Expression.definition
    -> Context.t
    -> (Context.t, Build_problem.t) result

val add_inductive:
    Source_entry.inductive array
    -> Context.t
    -> (Context.t, Build_problem.t) result


val add_entry:
    Ast.Source_entry.t
    -> Context.t
    -> (Context.t, Build_problem.t) result
