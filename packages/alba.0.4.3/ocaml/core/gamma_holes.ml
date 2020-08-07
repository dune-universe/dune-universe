open Fmlib
open Common


type term_n = Term.t * int

module Entry =
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
    entries: Entry.t array;
    bounds: (int * bool) array;          (* level of bound, is typed? *)
    nholes: int;
}



let make (base: Gamma.t): t =
    {
        base0 = base;
        base;
        entries = [||];
        bounds = [||];
        nholes = 0;
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


let count_entries (gh: t): int =
    Array.length gh.entries


let context (gh: t): Gamma.t =
    gh.base


let base_context (gh: t): Gamma.t =
    gh.base0

let is_valid_index (idx: int) (gh: t): bool =
    Gamma.is_valid_index idx gh.base

let name_of_index (idx: int) (gh: t): string =
    Gamma.name_of_index idx gh.base


let index_of_level (level: int) (gh: t): int =
    Gamma.index_of_level level gh.base


let level_of_index (idx: int) (gh: t): int =
    Gamma.level_of_index idx gh.base



let is_entry (idx: int) (gh: t): bool =
    idx < count_entries gh



let entry_of_index (idx: int) (gh: t): Entry.t =
    assert (is_entry idx gh);
    let level = level_of_index idx gh in
    gh.entries.(level - count_base gh)



let is_hole (idx: int) (gh: t): bool =
    is_entry idx gh
    && Entry.is_hole (entry_of_index idx gh)


let is_unfilled (idx: int) (gh: t): bool =
    is_entry idx gh
    &&
    Entry.is_unfilled (entry_of_index idx gh)



let is_bound (idx: int) (gh: t): bool =
    is_entry idx gh
    &&
    Entry.is_bound (entry_of_index idx gh)




let bound_number (idx: int) (gh: t): int =
    assert (is_bound idx gh);
    Entry.bound_number (entry_of_index idx gh)



let variable_of_bound (i: int) (gh: t): Term.t =
    assert (i < count_bounds gh);
    Term.Variable
            (index_of_level
                (fst gh.bounds.(i))
                gh)




let value (idx: int) (gh: t): Term.t option =
    if is_entry idx gh then
        Option.map
            (fun (term, n) ->
                assert (n <= count gh);
                Term.up (count gh - n) term)
            (Entry.value (entry_of_index idx gh))
    else
        None



let has_value (idx: int) (gh: t): bool =
    Option.has (value idx gh)


let collect_holes
    (cnt0: int)
    (filled: bool)
    (term: Term.t)
    (gh: t)
    : Int_set.t
    =
    (* collect filled or unfilled holes in [term] starting from [cnt0]. *)
    let cntbase =count_base gh
    in
    let delta = max (cnt0 - cntbase) 0
    in
    let idx_beyond = count_entries gh - delta
    in
    Term.fold_free
        (fun idx set ->
            if idx < idx_beyond then
                let entry = entry_of_index idx gh
                in
                if
                    Entry.is_hole entry
                    && ((Entry.value entry <> None) = filled)
                then
                    Int_set.add
                        (Gamma.level_of_index idx gh.base)
                        set
                else
                    set
            else
                set)
        term
        Int_set.empty




let unfilled_holes (cnt0: int) (term: Term.t) (gh: t): Int_set.t =
    collect_holes cnt0 false term gh




let expand (term: Term.t) (gh: t): Term.t =
    Term.substitute_with_beta
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





let name_at_level (level: int) (gh: t): string =
    Gamma.name_at_level level gh.base


let type_at_level (level: int) (gh: t): Term.typ =
    let typ = Gamma.type_at_level level gh.base in
    if count_base gh <= level then
        expand typ gh
    else
        typ


let type_of_variable (idx: int) (gh: t): Term.typ =
    type_at_level (Gamma.level_of_index idx gh.base) gh




let type_of_literal (value: Term.Value.t) (gh: t): Term.typ =
    Gamma.type_of_literal value gh.base



let definition_term (idx: int) (gh: t): Term.t option =
    Gamma.definition_term idx gh.base



let fold_entries
    (f: int -> int -> string -> Term.typ -> bool -> Term.t option -> 'a -> 'a)
    (gh: t)
    (a: 'a):
    'a
=
    let cnt0 = count_base gh
    in
    Array.foldi_left
        (fun a k entry ->
            let level = cnt0 + k in
            let idx = index_of_level level gh in
            f
                level
                idx
                (name_at_level level gh)
                (type_at_level level gh)
                (Entry.is_hole entry)
                (value idx gh)
                a
            )
        a
        gh.entries


let push_bound (name: string) (typed: bool) (typ: Term.typ) (gh: t): t =
    {gh with
        base = Gamma.push_local name typ gh.base;

        entries =
            Array.push
                (Entry.make_bound (Array.length gh.bounds))
                gh.entries;

        bounds = Array.push (count gh, typed) gh.bounds;
    }



let remove_bounds (n: int) (gh: t): t =
    assert (n <= count_bounds gh);
    {gh with bounds = Array.remove_last n gh.bounds}



let push_local (name: string) (typ: Term.typ) (gh: t): t =
    push_bound name true typ gh






let push_named_hole (name: string) (typ: Term.typ) (gh: t): t =
    {gh with
        base   = Gamma.push_local name typ gh.base;
        entries = Array.push Entry.hole gh.entries;
        nholes = gh.nholes + 1;
    }




let push_hole (typ: Term.typ) (gh: t): t =
    push_named_hole
        ("<" ^ string_of_int gh.nholes ^ ">")
        typ
        gh




let fill_hole0 (idx: int) (value: Term.t) (beta_reduce: bool) (gh: t): t =
    assert (is_unfilled idx gh);
    assert (not (Term.has_variable idx value));
    let value = expand value gh
    and cnt    = count gh
    and nentries  = count_entries gh
    and entries = Array.copy gh.entries
    in
    let cnt0 = cnt - nentries
    and loc_level = Term.bruijn_convert idx nentries
    in
    let gh_new = {gh with entries}
    in
    (* fill the hole *)
    entries.(loc_level) <-
        Entry.set_value (value, cnt) entries.(loc_level);

    (* [idx] and users of [idx] also become users of all unfilled holes in
    [value] *)
    let users = Entry.users entries.(loc_level) in
    Int_set.iter
        (fun unfilled ->
            let iloc = unfilled - cnt0 in
            entries.(iloc) <-
                Entry.add_users (cnt0 + loc_level) users entries.(iloc))
        (unfilled_holes cnt0 value gh);

    (* Substitute in all users of [idx] the variable [idx] by value. *)
    Int_set.iter
        (fun user ->
            let i = user - cnt0 in
            match Entry.value entries.(i) with
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
                entries.(i) <- Entry.set_value (term, cnt) entries.(i)
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
    let nentries = count_entries gh
    in
    Term.substitute
        (fun idx ->
            if nentries <= idx then
                Variable (idx + nb)
            else
                let entry = entry_of_index idx gh in
                if Entry.is_bound entry then
                    let i = Entry.bound_number entry in
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
