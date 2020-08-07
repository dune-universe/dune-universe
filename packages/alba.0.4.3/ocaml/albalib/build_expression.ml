open Fmlib
open Common
open Alba_core
open Ast



type type_in_context = Build_context.type_in_context




let description_of_type_in_context
    (nargs: int)
    (lst: (type_in_context * type_in_context) list)
    : Build_problem.description
    =
    let open Build_problem
    in
    if 0 < nargs then
        Not_a_function (List.map snd lst)
    else
        Wrong_type lst


module Name_map = Name_map
module Result = Fmlib.Result.Make (Build_problem)
module List_fold = List.Monadic (Result)
module Interval_monadic = Interval.Monadic (Result)
module Algo = Gamma_algo.Make (Gamma)


type t = {
    names: Name_map.t;
    base:  Gamma.t;
    bcs:   Build_context.t list;
}

let count_base (builder: t): int =
    Gamma.count builder.base




let push_bound (name: string) (builder: t): t =
    {builder with
        names = Name_map.add_local name builder.names}


let set_names
    (names: Name_map.t)
    (builder: t)
    : (t, Build_problem.t) result
=
    Ok {builder with names}



let make (c: Context.t): t =
    {
        names = Context.name_map c;
        base  = Context.gamma c;
        bcs   = [Build_context.make (Context.gamma c)]
    }





type 'a build_monad =
    t -> ('a, Build_problem.t) result



let map_bcs_list (f: Build_context.t -> Build_context.t) (builder: t): t =
    {builder with bcs = List.map f builder.bcs}



let map_bcs0
    (f: Build_context.t -> ('a, 'b) result)
    (g: 'b list -> Build_problem.t)
    : 'a list build_monad
    =
    fun builder ->
    let lst, errors =
        List.fold_left
            (fun (lst, errors)  bc ->
                match f bc with
                | Ok a ->
                    a :: lst, errors
                | Error problem ->
                    lst, problem :: errors)
            ([], [])
            builder.bcs
    in
    if lst <> [] then
        Ok lst
    else
        Error (g errors)





let map_bcs
    (f: Build_context.t -> (Build_context.t, 'a) result)
    (g: 'a list -> Build_problem.t)
    : t build_monad
    =
    fun builder ->
    Result.map
        (fun bcs -> {builder with bcs})
        (map_bcs0 f g builder)



let next_formal_argument
    (name: string Located.t)
    (typed: bool)
    (builder: t)
    : t
    =
    let str = Located.value name
    in
    map_bcs_list
        (Build_context.next_formal_argument name typed)
        builder
    |> push_bound str



let check_formal_arguments
    (fargs: Expression.formal_argument list)
    : t build_monad
    =
    map_bcs
        (Build_context.Product.check (List.length fargs))
        (fun lst ->
            let i_min =
                List.fold_left
                    (fun i_min i -> min i_min i)
                    (List.length fargs)
                    lst
            in
            let name, _ = List.nth_strict i_min fargs in
            Located.range name, Cannot_infer_bound)


let end_product
    (range: range)
    (nargs_outer: int)
    (nargs_inner: int)
    : t build_monad
    =
    map_bcs
        (Build_context.Product.end_ nargs_outer nargs_inner)
        (fun lst ->
            range,
            description_of_type_in_context nargs_outer lst)



let base_candidates
    (range: range)
    (candidates: Term.t list)
    (nargs: int)
    : t build_monad
    =
    fun builder ->
    let candidates, _ =
        List.fold_right
            (fun term (candidates, variant) ->
                (term, variant) :: candidates,
                variant + 1)
            candidates
            ([], 0)
    in
    let bcs =
        List.(
            builder.bcs >>= fun bc ->
            candidates  >>= fun (term, variant) ->
            Option.to_list
                (Build_context.base_candidate
                    range variant term nargs bc))
    in
    if bcs = [] then
        let acts =
            List.map
                (fun (term, _) ->
                    [],
                    Algo.type_of_term term builder.base,
                    builder.base)
                candidates
        and reqs =
            List.map Build_context.required_type_in_context builder.bcs
        in
        if 0 < nargs then
            Error (range, Not_a_function acts)
        else
            Error (range, Wrong_base (reqs, acts))
    else
        Ok {builder with bcs}



let rec build0
    (exp: Expression.t)
    (nargs: int)
    : t build_monad
    =
    fun builder ->
    let open Expression in
    let range = Located.range exp in
    match
        Located.value exp
    with
    | Number str ->
        let lst = Term.number_values str in
        if lst = [] then
            Error (range, Overflow)
        else
            base_candidates range lst nargs builder

    | Char code ->
        base_candidates range [Term.char code] nargs builder

    | String str ->
        base_candidates range [Term.string str] nargs builder

    | Proposition ->
        base_candidates range [Term.proposition] nargs builder

    | Any ->
        base_candidates range [Term.any] nargs builder

    | Identifier name | Operator (name, _) ->
        let cnt_base = count_base builder in
        (
            match Name_map.find name builder.names with
            | [] ->
                Error (range, No_name)

            | [level] when cnt_base <= level ->
                map_bcs
                    (Build_context.bound (level - cnt_base) nargs)
                    (fun lst ->
                        range,
                        description_of_type_in_context nargs lst)
                    builder

            | lst ->
                base_candidates
                    range
                    (List.map
                        (fun level -> Gamma.variable_at_level level builder.base)
                        lst)
                    nargs
                    builder
        )

    | Typed (exp, tp) ->
        let open Result in
        (map_bcs_list Build_context.Typed.start builder
        |> build0 tp 0)
        >>= fun builder ->
        (map_bcs_list Build_context.Typed.expression builder
        |> build0 exp 0)
        >>=
        map_bcs
            (Build_context.Typed.end_ nargs)
            (fun lst ->
                range,
                description_of_type_in_context nargs lst)

    | Product (fargs, res) ->
        let open Result
        in
        let names = builder.names
        in
        build_signature
            fargs
            (Some res)
            (map_bcs_list Build_context.Product.start builder)
        >>=
        check_formal_arguments fargs
        >>=
        end_product range nargs (List.length fargs)
        >>= set_names names


    | Application (f, args) ->
        let open Result in
        (*  Get a position number for each argument and the total number of
            arguments. *)
        let nargs, args =
            List.fold_right
                (fun (arg, mode) (n,args) -> n + 1, (n,arg,mode) :: args)
                args
                (0, [])
        in
        (* Build the function term. *)
        build0
            f
            nargs
            (map_bcs_list
                (Build_context.Application.start nargs)
                builder)
        >>=
        (* Build the arguments. *)
        List_fold.fold_left
            (fun (n, arg, mode) builder ->
                let mode =
                    match mode with
                    | Expression.Normal ->
                        Term.Application_info.Normal
                    | Expression.Operand ->
                        assert (nargs = 1 || nargs = 2);
                        if nargs = 2 then
                            Term.Application_info.Binary
                        else
                            Term.Application_info.Unary
                in
                build0 arg 0 builder
                >>=
                map_bcs
                    (Build_context.Application.apply n mode)
                    (fun lst ->
                        (fst range, Located.end_ arg),
                        description_of_type_in_context n lst)
            )
            args


    | Function (fargs, res, exp) ->
        let open Result in
        let names = builder.names in
        build_signature
            fargs
            res
            (map_bcs_list Build_context.Lambda.start builder)
        >>= fun builder ->
        Ok (map_bcs_list Build_context.Lambda.inner builder)
        >>= fun builder ->
        build0 exp 0 builder
        >>=
        map_bcs
            (Build_context.Lambda.end_
                nargs
                (List.length fargs)
                (res <> None))
            (fun lst ->
                range,
                description_of_type_in_context nargs lst)
        >>= set_names names

    | Where (exp, defs) ->
        let open Result in
        let rec build_where defs builder =
            match defs with
            | [] ->
                build0 exp 0 builder
            | def :: defs ->
                let name, _, _, _ =
                    Located.value def
                in
                let str = Located.value name
                and names = builder.names
                in
                build_where
                    defs
                    (map_bcs_list
                        (Build_context.Where.start name)
                        builder
                    |> push_bound str)
                >>=
                map_bcs
                    Build_context.Where.end_inner
                    (fun lst -> range, description_of_type_in_context 1 lst)
                >>= set_names names
                >>=
                build_definition0 def
                >>=
                map_bcs
                    (Build_context.Where.end_ nargs)
                    (fun lst -> range, description_of_type_in_context 0 lst)

        in
        build_where defs builder

    | List _ ->
        Error (range, Not_yet_implemented "Literal list")


and build_formal_argument
    ((name, tp): Expression.formal_argument)
    : t build_monad
    =
    fun builder ->
    let next typed builder = next_formal_argument name typed builder
    in
    match tp with
    | None ->
        Ok (next false builder)
    | Some tp ->
        Result.map (next true) (build0 tp 0 builder)


and build_signature
    (fargs: Expression.formal_argument list)
    (res: Expression.t option)
    : t build_monad
    =
    fun builder ->
    let open Result in
    List_fold.fold_left
        build_formal_argument
        fargs
        builder
    >>= fun builder ->
    match res with
    | None ->
        Ok builder
    | Some res ->
        build0 res 0 builder


and build_definition0
    (def: Expression.definition)
    : t build_monad
=
    let open Expression
    in
    let name, fargs, res_tp, def_exp =
        Located.value def
    in
    if fargs = [] && res_tp = None then
        build0 def_exp 0
    else if fargs = [] then
        build0
            Located.(make
                (start name)
                (Typed (def_exp, Option.value res_tp))
                (end_ name))
        0
    else
        build0
            Located.(make
                (start name)
                (Function (fargs, res_tp, def_exp))
                (end_ name))
            0






let check_ambiguity
    (c: Context.t)
    (builder: t)
    : (Build_context.t, Build_problem.t) result
    =
    match builder.bcs with
    | [] ->
        assert false (* Cannot happen! *)
    | _ :: _ :: _ ->
        let range, base_terms =
            Build_context.find_last_ambiguous builder.bcs
        in
        Error (
            range,
            Ambiguous (
                List.map
                    (fun (_, typ) ->
                        [], typ, Context.gamma c)
                    base_terms
            )
        )
    | [bc] ->
        Ok bc



let check_formal_arguments
    (bc: Build_context.t)
    : (Build_context.t, Build_problem.t) result
    =
    match Build_context.find_first_untyped_formal bc with
    | None ->
        Ok bc

    | Some range ->
        Error (range, Cannot_infer_bound)


let check_name_violations
    (bc: Build_context.t)
    : (Build_context.t, Build_problem.t) result
    =
    match Build_context.find_first_name_violation bc with
    | None ->
        Ok bc

    | Some (range, case, kind) ->
        Error (range, Name_violation (case, kind))



let check_incomplete
    (range: range)
    (bc: Build_context.t)
    : (Term.t * Term.typ, Build_problem.t) result
    =
    match Build_context.final bc with
    | Error err ->
        Error (range, Incomplete_type err)

    | Ok (_, term, typ) ->
        Ok (term, typ)



let build
      (exp: Expression.t)
      (c: Context.t)
    : (Term.t * Term.typ, Build_problem.t) result
    =
    let open Result in
    build0 exp 0 (make c)
    >>=
    check_ambiguity c
    >>=
    check_formal_arguments
    >>=
    check_name_violations
    >>=
    check_incomplete (Located.range exp)



let build_definition
    (def: Expression.definition)
    (c: Context.t)
    : (Term.t * Term.typ, Build_problem.t) result
=
    let open Fmlib.Result in
    build_definition0 def (make c)
    >>=
    check_ambiguity c
    >>=
    check_formal_arguments
    >>=
    check_name_violations
    >>=
    check_incomplete (Located.range def)



let build_named_type
    (name: string Located.t)
    (exp: Expression.t)
    (c: Context.t)
    : (Term.typ, Build_problem.t) result
=
    let open Fmlib.Result in
    build exp c
    >>= fun (typ, _) ->
    match
        Algo.check_naming_convention
            (Located.value name)
            typ
            (Context.gamma c)
    with
    | Ok _ ->
        Ok typ
    | Error violation ->
        let case, kind =
            Gamma_algo.strings_of_violation violation
        in
        Error (
            Located.range name,
            Build_problem.Name_violation (case, kind)
        )



