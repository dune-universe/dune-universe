open Alba_core
open Ast


module Inductive_parser =
    Parser_lang.Make (
        struct
            type t = Source_entry.inductive array
        end)


module Expression_parser =
    Parser_lang.Make (Expression)


let add_inductive
    (src: string)
    (c: Context.t)
    : (Context.t, Build_problem.t) result
=
    let open Inductive_parser in
    let p = run (inductive_family ()) src
    in
    assert (has_ended p);
    assert (has_succeeded p);
    match result p with
    | None ->
        assert false
    | Some inds ->
        Builder.add_inductive inds c


let build_expression
    (src: string)
    (c: Context.t):
    (Term.t * Term.typ, Build_problem.t) result
=
    let open Expression_parser in
    let p = run (expression ()) src in
    assert (has_ended p);
    assert (has_succeeded p);
    match result p with
    | None ->
        assert false (* Syntax error *)
    | Some exp ->
        Build_expression.build exp c



(*  Test the inductive types
    ------------------------

    - same number of parameters
    - same parameter names
    - same parameter types
    - inductive type must be a type (i.e. its type a kind)
*)


let%test _ =
    let src = "class I0 A :=\n\
               class I1 A B :="
    in
    match
        add_inductive src Context.empty
    with
    | Error (_, Build_problem.Wrong_parameter_count _) ->
        true
    | _ ->
        false


let%test _ =
    let src = "class I0 A :=\n\
               class I1 B :="
    in
    match
        add_inductive src Context.empty
    with
    | Error (_, Build_problem.Wrong_parameter_name _) ->
        true
    | _ ->
        false



let%test _ =
    let src = "class I0 (A: Any -> Any) :=\n\
               class I1 A :="
    in
    match
        add_inductive src Context.empty
    with
    | Error (_, Build_problem.Wrong_parameter_type _) ->
        true
    | _ ->
        false


let%test _ =
    let src = "class i A: Int :="
    and context =
        Context.(
            empty
            |>
            add_builtin_type
                "int_type"
                "Int"
                Term.any
        )
    in
    match
        add_inductive src context
    with
    | Error (_, Build_problem.No_inductive_type) ->
        true
    | _ ->
        false


let%test _ =
    let src = "class I :=\n\
               class I :="
    in
    match
        add_inductive src Context.empty
    with
    | Error (_, Build_problem.Duplicate_inductive) ->
        true
    | _ ->
        false



(*  Test the constructors
    ------------------------

    - no duplicate constructor names
    - if there are indices, then type must be explicit
    - construct a object of the corresponding inductive type
    - positivity
    - positivity in family
    - positivity with other inductive type
*)

let%test _ =
    let src = "class I := c; c"
    in
    match
        add_inductive src Context.empty
    with
    | Error (_, Build_problem.Duplicate_constructor) ->
        true
    | _ ->
        false



let%test _ =
    let src = "class I A: A -> Any := constr"
    in
    match
        add_inductive src Context.empty
    with
    | Error (_, Build_problem.Missing_inductive_type) ->
        true
    | _ ->
        false



let%test _ =
    let src = "class I A := Constr: Any"
    in
    match
        add_inductive src Context.empty
    with
    | Error (_, Build_problem.Wrong_type_constructed _) ->
        true
    | _ ->
        false



let%test _ =
    let src = "class I A := constr: I Proposition"
    in
    match
        add_inductive src Context.empty
    with
    | Error (_, Build_problem.Wrong_type_constructed _) ->
        true
    | _ ->
        false



let%test _ =
    let src = "class I := constr: (I -> Proposition) -> I"
    in
    match
        add_inductive src Context.empty
    with
    | Error (_, Build_problem.Negative) ->
        true
    | _ ->
        false



let%test _ =
    let src = "class I A := constr: I (I A) -> I A"
    in
    match
        add_inductive src Context.empty
    with
    | Error (_, Build_problem.Not_positive _) ->
        true
    | _ ->
        false


let%test _ =
    let src1 = "class I1 A := co1: (A -> A) -> I1 A"
    and src2 = "class I := co : I1 I -> I"
    in
    match
        add_inductive src1 Context.empty
    with
    | Error _ ->
        false
    | Ok c ->
        match
            add_inductive src2 c
        with
        | Error (_, Build_problem.Nested_negative _) ->
            true
        | _ ->
            false


let%test _ =
    let open Fmlib.Result in
    let src1 = "Any -> Any"
    and src2 = "class II := co : TC II -> II"
    in
    match
        (
            build_expression src1 Context.empty
            >>= fun (typ, _) ->
            add_inductive src2 (Context.(add_builtin_type "TC" "TC" typ empty))
        )
    with
    | Error (_, Not_positive _) ->
        true
    | _ ->
        false
