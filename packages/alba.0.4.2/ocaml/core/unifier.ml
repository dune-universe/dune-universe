open Fmlib


module type HOLES =
sig
    include Gamma_algo.GAMMA

    val context: t -> Gamma.t

    val expand: Term.t -> t -> Term.t

    val is_hole: int -> t -> bool

    val value: int -> t -> Term.t option

    val fill_hole0: int -> Term.t -> bool -> t -> t
end


module Make (GH: HOLES) =
struct
    module Algo = Gamma_algo.Make (Gamma)

    type t = {
        gh: GH.t;
        gamma: Gamma.t
    }


    let make (gh: GH.t): t =
        {gh; gamma = GH.context gh}


    let context (uc: t): GH.t =
        uc.gh


    let push (name: string) (tp: Term.typ) (uc: t): t =
        {uc with gamma = Gamma.push_local name tp uc.gamma}


    let string_of_term (term: Term.t) (uc: t): string =
        Term_printer.string_of_term term uc.gamma
    let _ = string_of_term


    let delta (uc: t): int =
        Gamma.count uc.gamma - GH.count uc.gh


    let is_hole (idx: int) (uc: t): bool =
        let nb = delta uc in
        if idx < nb then
            false
        else
            GH.is_hole (idx - delta uc) uc.gh


    let expand (term: Term.t) (uc: t): Term.t =
        let del = delta uc
        in
        Term.substitute
            (fun i ->
                if i < del then
                    Variable i
                else
                    match GH.value (i - del) uc.gh with
                    | None ->
                        Variable i
                    | Some term ->
                        Term.up del term)
            term



    type unifier = Term.typ -> Term.typ -> bool -> t -> t option


    let set
        (i: int) (typ: Term.typ) (beta_reduce: bool) (uni: unifier) (uc: t)
        : t option
        =
    (* Fill the hole [i] with [typ] if their types can be unified. *)
        let open Option in
        let nb = delta uc in
        Term.down nb typ
        >>= fun typ0 ->
            (* typ does not contain any new bound variables!!i
               typ0 is valid in gh,
               (i - nb) is the hole in gh *)
        map
            (fun uc ->
                {uc with gh = GH.fill_hole0 (i - nb) typ0 beta_reduce uc.gh})
            (uni
                (Algo.type_of_term typ uc.gamma)
                (Gamma.type_of_variable i uc.gamma)
                true
                uc)


    let setf
        (f: int) (arg: Term.t) (typ: Term.typ) (uni: unifier) (uc: t)
        : t option
        =
        (* Unify [f arg] with [typ] where [f] is a hole, i.e. assign [\lam x :=
        typ] to [f]. *)
        let fterm =
            let arg_tp = Algo.type_of_term arg uc.gamma
            and exp =
                match arg with
                | Variable i ->
                    Term.map
                        (fun j ->
                            if i = j then
                                0
                            else
                                j + 1)
                        typ
                | _ ->
                    Term.up1 typ
            in
            Term.lambda "_" arg_tp exp
        in
        set f fterm true uni uc



    let rec unify0
        (act: Term.typ)
        (req: Term.typ)
        (is_super: bool)
        (uc: t)
        : t option
        =
        let req = Algo.key_normal (expand req uc) uc.gamma
        and act = Algo.key_normal (expand act uc) uc.gamma
        and nb = delta uc
        and set i typ = set i typ false unify0 uc
        in
        let open Term
        in
        match act, req with
        | Sort act, Sort req
            when (is_super && Sort.is_super req act)
                 || (not is_super && req = act)
            ->
                Some uc

        | Value act, Value req ->
            if Value.is_equal act req then
                Some uc
            else
                None

        | Appl (f_act, arg_act, _ ), Appl (f_req, arg_req, _) ->
            let open Option in
            unify0 f_act f_req false uc
            >>=
            unify0 arg_act arg_req false

        | Pi (act_arg, act_rt, info), Pi (req_arg, req_rt, _) ->
            Option.(
                unify0 act_arg req_arg false uc
                >>= fun uc ->
                let gamma = uc.gamma in
                map
                    (fun uc -> {uc with gamma})
                    (unify0
                        act_rt
                        req_rt
                        is_super
                        (push (Pi_info.name info) act_arg uc))
            )

        | Variable i, Variable j ->
            if i = j then
                Some uc
            else if i < nb || j < nb then
                None
            else
                let i_hole = is_hole i uc
                and j_hole = is_hole j uc
                in
                if not (i_hole || j_hole) then
                    None
                else if i_hole && j_hole then
                    match set j act with
                    | None ->
                        set i req
                    | res ->
                        res
                else if i_hole then
                    set i req
                else if j_hole then
                    set j act
                else
                    assert false (* cannot happen, illegal path *)

        | Appl (Variable f, arg, _), _  when is_hole f uc ->
            setf f arg req unify0 uc

        | _, Appl (Variable f, arg, _ ) when is_hole f uc ->
            setf f arg act unify0 uc

        | Variable i, _ when is_hole i uc ->
            set i req

        | _, Variable j when is_hole j uc ->
            set j act

        | _, _ ->
            None




    let unify
        (act: Term.typ)
        (req: Term.typ)
        (is_super: bool)
        (gh: GH.t)
        : GH.t option
        =
        Option.map
            (fun uc -> uc.gh)
            (unify0 act req is_super (make gh))

end
