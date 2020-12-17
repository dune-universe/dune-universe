open Alba_core
open Ast


val build:
    Expression.t
    -> Context.t
    -> (Term.t * Term.typ, Build_problem.t) result


val build_definition:
    Expression.definition
    -> Context.t
    -> (Term.t * Term.typ, Build_problem.t) result


val build_named_type:
    string Located.t
    -> Expression.t
    -> Context.t
    -> (Term.typ, Build_problem.t) result
