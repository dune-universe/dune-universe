open Fmlib
open Common


type term_n = Term.t * int

module Local =
struct
    type t =
        | Hole of
            term_n option
            * Int_set.t (* Users *)
        | Bound of int  (* number of bound variable (counting upwards) *)

    let hole: t =
        Hole (None, Int_set.empty)

    let make_bound (n: int): t =
        Bound n

    let is_hole (loc: t): bool =
        match loc with
        | Hole _ ->
            true
        | Bound _ ->
            false

    let is_unfilled (loc: t): bool =
        match loc with
        | Hole (None, _ ) ->
            true
        | _ ->
            false

    let is_bound (loc: t): bool =
        not (is_hole loc)


    let value (loc: t): term_n option =
        match loc with
        | Hole (value, _) ->
            value
        | _ ->
            None

    let users (loc: t): Int_set.t =
        match loc with
        | Hole (_, users) ->
            users
        | _ ->
            assert false (* Illegal call! *)


    let add_users (user: int) (users: Int_set.t) (loc: t): t =
        match loc with
        | Hole (value, users0) ->
            let set = Int_set.add user (Int_set.union users0 users) in
            Hole (value, set)
        | _ ->
            assert false (* Illegal call! *)


    let set_value (term_n: term_n) (loc: t): t =
        match loc with
        | Hole (_, users) ->
            Hole (Some term_n, users)
        | _ ->
            assert false (* Illegal call! *)


    let bound_number (loc: t): int =
        match loc with
        | Bound n ->
            n
        | _ ->
            assert false (* Illegal call! *)
end





type t = {
    base0: Gamma.t;
    base: Gamma.t;
    locals: Local.t array;
    bounds: (int * bool) array;          (* level of bound, is typed? *)
    nholes: int;
}



let make (base: Gamma.t): t =
    {
        base0 = base;
        base;
        locals = [||];
        bounds = [||];
        nholes = 0
    }


let string_of_term (term: Term.t) (gh: t): string =
    Term_printer.string_of_term term gh.base
let _ = string_of_term



let count (gh: t): int =
    Gamma.count gh.base


let count_base (gh: t): int =
    Gamma.count gh.base0


let count_bounds (gh: t): int =
    Array.length gh.bounds


let count_locals (gh: t): int =
    Array.length gh.locals


let context (gh: t): Gamma.t =
    gh.base


let base_context (gh: t): Gamma.t =
    gh.base0


let index_of_level (level: int) (gh: t): int =
    Gamma.index_of_level level gh.base


let level_of_index (idx: int) (gh: t): int =
    Gamma.level_of_index idx gh.base


let local_of_index (idx: int) (gh: t): Local.t =
    let nlocs = count_locals gh
    in
    assert (idx < nlocs);
    gh.locals.(Term.bruijn_convert idx nlocs)


let is_hole (idx: int) (gh: t): bool =
    idx < count_locals gh
    && Local.is_hole (local_of_index idx gh)


let is_unfilled (idx: int) (gh: t): bool =
    let level = level_of_index idx gh
    and cnt0 = count_base gh in
    cnt0 <= level
    &&
    let iloc = level - cnt0 in
    Local.is_unfilled gh.locals.(iloc)



let is_bound (idx: int) (gh: t): bool =
    idx < count_locals gh
    && Local.is_bound (local_of_index idx gh)




let bound_number (idx: int) (gh: t): int =
    assert (is_bound idx gh);
    Local.bound_number (local_of_index idx gh)



let variable_of_bound (i: int) (gh: t): Term.t =
    assert (i < count_bounds gh);
    Term.Variable
            (index_of_level
                (fst gh.bounds.(i))
                gh)


let value (idx: int) (gh: t): Term.t option =
    let nlocs = count_locals gh
    in
    if nlocs <= idx then
        None
    else
        Option.map
            (fun (term, n) ->
                assert (n <= count gh);
                Term.up (count gh - n) term)
            (Local.value (local_of_index idx gh))



let has_value (idx: int) (gh: t): bool =
    Option.has (value idx gh)



let collect_holes (cnt0: int) (filled: bool) (term: Term.t) (gh: t): Int_set.t =
    let cnt = count gh
    and nlocs = count_locals gh
    in
    assert (cnt0 <= cnt);
    let nmin = min nlocs (cnt - cnt0)
    in
    Term.fold_free
        (fun idx set ->
            if nmin <= idx then
                set
            else
                let loc = local_of_index idx gh in
                if Local.is_hole loc && ((Local.value loc <> None) = filled) then
                    Int_set.add
                        (Gamma.level_of_index idx gh.base)
                        set
                else
                    set)
        term
        Int_set.empty




let unfilled_holes (cnt0: int) (term: Term.t) (gh: t): Int_set.t =
    collect_holes cnt0 false term gh




let expand (term: Term.t) (gh: t): Term.t =
    Term.substitute
        (fun i ->
            match value i gh with
            | None ->
                Variable i
            | Some term ->
                term)
        term


let is_expanded (term: Term.t) (gh: t): bool =
    Int_set.is_empty
        (collect_holes 0 true term gh)



let term_of_term_n ((term,n): Term.t_n) (gh: t): Term.t =
    expand (Term.up (count gh - n) term) gh





let type_at_level (level: int) (gh: t): Term.typ =
    let typ = Gamma.type_at_level level gh.base in
    if count_base gh <= level then
        expand typ gh
    else
        typ


let type_of_variable (idx: int) (gh: t): Term.typ =
    type_at_level (Gamma.level_of_index idx gh.base) gh



let name_at_level (level: int) (gh: t): string =
    Gamma.name_at_level level gh.base



let type_of_literal (value: Term.Value.t) (gh: t): Term.typ =
    Gamma.type_of_literal value gh.base



let definition_term (idx: int) (gh: t): Term.t option =
    Gamma.definition_term idx gh.base



let push_bound (name: string) (typed: bool) (typ: Term.typ) (gh: t): t =
    {gh with
        base = Gamma.push_local name typ gh.base;

        locals =
            Array.push
                (Local.make_bound (Array.length gh.bounds))
                gh.locals;

        bounds = Array.push (count gh, typed) gh.bounds;
    }



let remove_bounds (n: int) (gh: t): t =
    assert (n <= count_bounds gh);
    {gh with bounds = Array.remove_last n gh.bounds}



let push_local (name: string) (typ: Term.typ) (gh: t): t =
    push_bound name true typ gh



let push_hole (typ: Term.typ) (gh: t): t =
    let name = "<" ^ string_of_int gh.nholes ^ ">"
    in
    {gh with
        base   = Gamma.push_local name typ gh.base;
        locals = Array.push Local.hole gh.locals;
        nholes = gh.nholes + 1;
    }




let fill_hole0 (idx: int) (value: Term.t) (beta_reduce: bool) (gh: t): t =
    assert (is_unfilled idx gh);
    let value = expand value gh
    and cnt    = count gh
    and nlocs  = count_locals gh
    and locals = Array.copy gh.locals
    in
    let cnt0 = cnt - nlocs
    and loc_level = Term.bruijn_convert idx nlocs
    in
    let gh_new = {gh with locals}
    in
    (* fill the hole *)
    locals.(loc_level) <-
        Local.set_value (value, cnt) locals.(loc_level);

    (* [idx] and users of [idx] also become users of all unfilled holes in
    [value] *)
    let users = Local.users locals.(loc_level) in
    Int_set.iter
        (fun unfilled ->
            let iloc = unfilled - cnt0 in
            locals.(iloc) <-
                Local.add_users (cnt0 + loc_level) users locals.(iloc))
        (unfilled_holes cnt0 value gh);

    (* Substitute in all users of [idx] the variable [idx] by value. *)
    Int_set.iter
        (fun user ->
            let i = user - cnt0 in
            match Local.value locals.(i) with
            | Some (term,n) ->
                let term = Term.up (count gh - n) term in
                let term =
                    Term.substitute0
                        (fun k ->
                            if k = idx then
                                value
                            else
                                Term.Variable k)
                        beta_reduce
                        term
                in
                locals.(i) <- Local.set_value (term, cnt) locals.(i)
            | _ ->
                assert false (* Illegal, all users must have a substitution,
                otherwise they would not be users. *))
        users;
    gh_new



let fill_hole (idx: int) (value: Term.t) (gh: t): t =
    fill_hole0 idx value false gh



let into_binder
    (bnd0: int)
    (nb: int)
    (term: Term.t)
    (gh: t)
    : Term.typ
    =
    (* Put [term] into a context with additional [nb] bound variables. The bound
    variables [bnd0, bnd0+1, ..., bnd0+nb-1] become the new bound variables
    [0,1,...,nb-1]. *)
    assert (bnd0 <= count_bounds gh);
    let nlocs = count_locals gh
    in
    Term.substitute
        (fun idx ->
            if nlocs <= idx then
                Variable (idx + nb)
            else
                let loc = local_of_index idx gh in
                if Local.is_bound loc then
                    let i = Local.bound_number loc in
                    if bnd0 <= i then
                    (
                        assert (i < bnd0 + nb);
                        Variable (Term.bruijn_convert (i - bnd0) nb)
                    )
                    else
                        Variable (idx + nb)
                else
                    Variable (idx + nb))
        term


let pi_lambda
    (mk: string -> bool -> Term.typ -> Term.t -> Term.t)
    (nbounds: int)
    (inner: Term.t)
    (gh: t)
    : Term.t
    =
    assert (nbounds <= count_bounds gh);
    let bnd0 = count_bounds gh - nbounds in
    let into = into_binder bnd0 in
    let rec make i exp =
        if i = 0 then
            exp
        else
            let i = i - 1 in
            let name, typed, arg_tp =
                let level, typed = gh.bounds.(bnd0 + i) in
                name_at_level level gh,
                typed,
                into i (type_at_level level gh) gh
            in
            make i (mk name typed arg_tp exp)
    in
    make nbounds (into nbounds inner gh)




let pi (nargs: int) (res_tp: Term.typ) (gh: t): Term.typ =
    assert (0 < nargs);
    assert (nargs <= count_bounds gh);
    pi_lambda Term.product0 nargs res_tp gh




let lambda (nargs: int) (exp: Term.t) (gh: t): Term.t =
    assert (0 < nargs);
    assert (nargs <= count_bounds gh);
    pi_lambda Term.lambda0 nargs exp gh
