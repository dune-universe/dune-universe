open Ast
open Alba_core

val build:
    Source_entry.inductive array
    -> Context.t
    -> (Inductive.t, Build_problem.t) result
