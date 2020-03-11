open Fmlib

module Algo = Gamma_algo.Make (Gamma_holes)

module Uni = Unifier.Make (Gamma_holes)


include Gamma_holes

let string_of_term term gh =
    Term_printer.string_of_term term (Gamma_holes.context gh)
let _ = string_of_term


let is_subtype (sub: Term.typ) (typ: Term.typ) (gh: t) : bool
    =
    Option.has (Uni.unify sub typ true gh)





let rec check (term: Term.t) (c: t): Term.typ option =
    let open Term in
    let open Option in
    match term with
    | Sort s ->
        Some (type_of_sort s)

    | Value v ->
        Some (type_of_literal v c)

    | Variable i ->
        if i < count c then
            Some (type_of_variable i c)
        else
            None

    | Appl (f, arg, _ ) ->
        check f c >>= fun f_type ->
        check arg c >>= fun arg_type ->
        (
            match Algo.key_normal f_type c with
            | Pi (tp, rt, _ ) when is_subtype arg_type tp c ->
                Some (apply rt arg)
            | _ ->
              None
        )

    | Typed (exp, tp) ->
        check exp c >>=
        fun exp_type ->
        check tp c >>=
        fun tp_tp ->
        (   match tp_tp with
            | Sort _  when is_subtype exp_type tp c ->
                Some tp
            | _ ->
                None
        )

    | Lambda (arg, exp, info ) ->
        check arg c >>= fun sort ->
        if Term.is_sort sort then
            let name = Lambda_info.name info in
            check exp (push_local name arg c)
            >>= fun res ->
            Some (
                Pi (arg, res, Pi_info.typed name)
            )
        else
            None

    | Pi (arg, res, info) ->
        check arg c >>=
        (
            function
            | Sort arg_sort ->
                check res (push_local (Pi_info.name info) arg c)
                >>=
                (
                    function
                    | Sort res_sort ->
                        Some (Sort (Sort.pi_sort arg_sort res_sort))
                    | _ ->
                        None
                )
            | _ ->
                None
        )




let check (term: Term.t) (gamma: Gamma.t): Term.typ option =
    check term (make gamma)



let is_valid_context (gamma: Gamma.t): bool =
    let cnt = Gamma.count gamma in
    let rec check_entry i =
        if i = cnt then
            true
        else
            let typ = Gamma.type_at_level i gamma in
            match Term.down (cnt - i) typ with
            | None ->
                false
            | Some _ ->
                match check typ gamma with
                | None ->
                    false
                | Some _ ->
                    let idx = Gamma.index_of_level i gamma in
                    match Gamma.definition_term idx gamma with
                    | None ->
                        check_entry (i + 1)
                    | Some def ->
                        match Term.down (cnt - i) def with
                        | None ->
                            false
                        | Some _ ->
                            match check def gamma with
                            | None ->
                                false
                            | Some def_typ ->
                                let gh = Gamma_holes.make gamma in
                                is_subtype def_typ typ gh
                                && check_entry (i + 1)
    in
    check_entry 0


let%test _ =
    is_valid_context (Gamma.standard ())
