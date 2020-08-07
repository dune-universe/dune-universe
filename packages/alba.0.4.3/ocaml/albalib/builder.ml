open Fmlib
open Common
open Alba_core
open Ast

module Algo = Gamma_algo.Make (Gamma)

module Parser     = Parser_lang
module Position   = Character_parser.Position


type pos = Position.t

type range = pos * pos

module Located =
  Character_parser.Located




let add_definition
    (def: Expression.definition)
    (context: Context.t)
    : (Context.t, Build_problem.t) result
=
    let open Fmlib.Result in
    let name, _, _, _ = Located.value def
    in
    Build_expression.build_definition def context
    >>=
    (   fun (term, typ) ->
        match
            Algo.check_naming_convention
                (Located.value name)
                typ
                (Context.gamma context)
        with
        | Ok () ->
            Ok (term, typ)
        | Error violation ->
            let case, kind = Gamma_algo.strings_of_violation violation
            in
            Error (Located.range name, Name_violation (case, kind))
    )
    >>= fun (term, typ) ->
    match
        Context.add_definition
            (Located.value name)
            typ
            term
            context
    with
    | Error _ ->
        Error (Located.range name, Ambiguous_definition)

    | Ok context ->
        Ok context




module Name_map = Name_map
module Result = Fmlib.Result.Make (Build_problem)
module Interval_monadic = Interval.Monadic (Result)


let add_inductive
    (inds: Source_entry.inductive array)
    (c: Context.t)
    : (Context.t, Build_problem.t) result
=
    let open Result in
    Build_inductive.build inds c
    >>= fun ind ->
    Ok (Context.add_inductive ind c)



let add_entry
    (entry: Source_entry.t)
    (c: Context.t)
    : (Context.t, Build_problem.t) result
=
    match entry with
    | Source_entry.Normal def ->
        add_definition def c

    | Source_entry.Inductive inds ->
        add_inductive inds c
