open Fmlib

module Algo = Gamma_algo.Make (Gamma_holes)

module Uni = Unifier.Make (Gamma_holes)


include Gamma_holes
    (* We use [Gamma_holes] as a normal [Gamma]. The type checker does not need
    any holes, but the unifier expects a parameter which allows for filling
    holes. However, if there are no holes, the unifier cannot fill any. We use
    the unifier here only to check, if a type is a subtype of another. *)



module Problem =
struct
    type t =
    | Out_of_bound of int * Gamma.t
    | Argument_type
    | Typed
    | Lambda
    | No_type
    | Name of string
end


type path = int list


module Result = Monad.Result (struct type t = path * Problem.t end)


let string_of_term term gh =
    Term_printer.string_of_term term (Gamma_holes.context gh)
let _ = string_of_term



let is_subtype (sub: Term.typ) (typ: Term.typ) (gh: t) : bool
    =
    Option.has (Uni.unify sub typ true gh)




let check_variable_name
    (name: string)
    (sort_of_type: Term.Sort.t)
    : bool
    =
    let open Term.Sort in
    let open Common in
    let letter, lower, upper =
        if String.length name > 0 then
            let c = name.[0] in
            Char.( is_letter c, is_lower c, is_upper c )
        else
            false, false, false
    in
    match sort_of_type with
    | Any i when 0 < i && (not letter || upper)->
        true
    | Proposition | Any 0 when not letter || lower ->
        true
    | _ ->
        false




let rec check
    (path: path)
    (term: Term.t)
    (c: t)
    : (Term.typ, path * Problem.t) result
    =
    let open Term in
    let open Result in
    match term with
    | Sort s ->
        Ok (type_of_sort s)

    | Value v ->
        Ok (type_of_literal v c)

    | Variable i ->
        if i < count c then
            Ok (type_of_variable i c)
        else
            Error (path, Problem.Out_of_bound (i, context c))

    | Appl (f, arg, _ ) ->
        check (0 :: path) f c
        >>= fun f_type ->
        check (1 :: path) arg c
        >>= fun arg_type ->
        (
            match Algo.key_normal f_type c with
            | Pi (tp, rt, _ ) when is_subtype arg_type tp c ->
                Ok (apply rt arg)

            | _ ->
              Error (path, Problem.Argument_type)
        )

    | Typed (exp, tp) ->
        check (0 :: path) exp c
        >>=
        fun exp_type ->
        check (1 :: path) tp c >>=
        fun tp_tp ->
        (   match tp_tp with
            | Sort _  when is_subtype exp_type tp c ->
                Ok tp

            | _ ->
                Error (path, Problem.Typed)
        )

    | Lambda (arg, exp, info ) ->
        check (0 :: path) arg c
        >>= fun sort ->
        (
            match sort with
            | Sort sort ->
                let name = Lambda_info.name info in
                if check_variable_name name sort then
                    check (1 :: path) exp (push_local name arg c)
                    >>= fun res ->
                    Ok ( Pi (arg, res, Pi_info.typed name) )
                else
                    Error (path, Problem.Name name)
            | _ ->
                Error (path, Problem.Lambda)
        )

    | Pi (arg, res, info) ->
        check (0 :: path) arg c
        >>=
        (
            function
            | Sort arg_sort ->
                let name = Pi_info.name info in
                if check_variable_name name arg_sort then
                    check
                        (1 :: path)
                        res
                        (push_local (Pi_info.name info) arg c)
                    >>=
                    (
                        function
                        | Sort res_sort ->
                            Ok (Sort (Sort.pi_sort arg_sort res_sort))
                        | _ ->
                            Error (path, Problem.No_type)
                    )
                else
                    Error (path, Problem.Name name)
            | _ ->
                Error (path, Problem.No_type)
        )

    | Where (name, tp, exp, def) ->
        check (0 :: path) tp c
        >>= fun tp_tp ->
        (
            match tp_tp with
            | Sort sort ->
                if check_variable_name name sort then
                    check (1 :: path) exp (push_local name tp c)
                    >>= fun res_tp ->
                    check (2 :: path) def c
                    >>= fun def_type ->
                    if is_subtype def_type tp c then
                        Ok (apply res_tp def)
                    else
                        Error (path, Problem.Argument_type)
                else
                    Error (path, Problem.Name name)
            | _ ->
                Error (path, Problem.No_type)
        )
        (*
        check path (expand_where name tp exp def) c*)




let equivalent (tp1: Term.typ) (tp2: Term.typ) (gamma: Gamma.t): bool =
    Option.has (Uni.unify tp1 tp2 false (make gamma))




let check
    (term: Term.t)
    (gamma: Gamma.t)
    : (Term.typ, path * Problem.t) result
    =
    check [] term (make gamma)



(* TODO: turn printf statements into error messages? *)
let is_valid_context (gamma: Gamma.t): bool =
    let cnt = Gamma.count gamma in
    let rec check_entry i =
        if i = cnt then
            true
        else
            let typ = Gamma.type_at_level i gamma in
            match Term.down (cnt - i) typ with
            | None ->
               (* This cannot happen by construction: type_at_level calls
                  Term.up (cnt - i) *)
                Printf.printf "variables out of bound\n";
                false
            | Some _ ->
                match check typ gamma with
                | Error _ ->
                    Printf.printf "type of level %d invalid\n %s\n"
                        i
                        (Term_printer.string_of_term typ gamma)
                        ;

                    false
                | Ok _ ->
                   (* TODO: check that type is a type (and not an object) *)
                    let idx = Gamma.index_of_level i gamma in
                    match Gamma.definition_term idx gamma with
                    | None ->
                        check_entry (i + 1)
                    | Some def ->
                        match Term.down (cnt - i) def with
                        (* This cannot happen by construction: definition_term calls
                           Term.up (cnt - i) *)
                        | None ->
                            false
                        | Some _ ->
                            match check def gamma with
                            | Error _ ->
                                false
                            | Ok def_typ ->
                                let gh = Gamma_holes.make gamma in
                                is_subtype def_typ typ gh
                                && check_entry (i + 1)
    in
    check_entry 0
