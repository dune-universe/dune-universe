open Fmlib
open Common
open Alba_core


module Located = Character_parser.Located
type pos = Character_parser.Position.t
type range = pos * pos


module Algo = Gamma_algo.Make (Gamma_holes)
module Uni  = Unifier.Make (Gamma_holes)


module Stack =
struct
    let split (stack: 'a list): 'a * 'a list =
        match stack with
        | [] ->
            assert false (* Illegal call! *)
        | hd :: tl ->
            hd, tl

    let swap (sp: 'a) (stack: 'a list): 'a * 'a list =
        match stack with
        | hd :: tl ->
            hd, sp :: tl
        | _ ->
            assert false (* Illegal call! *)

    let rec pop (nargs: int) (sp: 'a) (stack: 'a list): 'a * 'a list =
        if nargs = 0 then
            sp, stack
        else
            match stack with
            | [] ->
                assert false (* Illegal call! *)
            | sp :: stack ->
                pop (nargs - 1) sp stack
end


module Implicits =
struct
    type t = int String_map.t

    let empty: t =
        String_map.empty

    let add (name: string) (impl: t): string * t =
        let make_name n name =
            name ^ "." ^ string_of_int n ^ "?"
        in
        match String_map.maybe_find name impl with
        | None ->
            name ^ "?",
            String_map.add name 1 impl

        | Some n ->
            assert (0 < n);
            make_name n name,
            String_map.add name (n + 1) impl
end



type entry = {
    cnt0: int;
}


type t = {
    gh: Gamma_holes.t;
    base_candidates: (range * int * Term.t) list;
    formal_arguments: (string Located.t * int) list; (* level of formal *)
    implicits: Implicits.t;
    sp: int;
    stack: int list;
    entry: entry;
    entries: entry list;
}


type type_in_context = int list * Term.typ * Gamma.t




let index_of_level (level: int) (bc: t): int =
    Gamma_holes.index_of_level level bc.gh


let string_of_term (term: Term.t) (bc: t): string =
    Term_printer.string_of_term term (Gamma_holes.context bc.gh)
let _ = string_of_term



let count (bc: t): int =
    Gamma_holes.count bc.gh

let count_base (bc: t): int =
    Gamma_holes.count_base bc.gh


let count_entries (bc: t): int =
    Gamma_holes.count_entries bc.gh


let count_bounds (bc: t): int =
    Gamma_holes.count_bounds bc.gh

let context (bc: t): Gamma.t =
    Gamma_holes.context bc.gh

let type_at_level (level: int) (bc: t): Term.typ =
    assert (level < count bc);
    Gamma_holes.type_at_level level bc.gh


let type_of_term (term: Term.t) (bc: t): Term.typ =
    Algo.type_of_term term bc.gh



let key_normal (term: Term.t) (bc: t): Term.t =
    Algo.key_normal term bc.gh


let is_kind (typ: Term.typ) (bc: t): bool =
    Algo.is_kind typ bc.gh



let required_type (bc: t): Term.typ =
    type_at_level bc.sp bc



let term_at_level (level: int) (bc: t): Term.t =
    Gamma_holes.expand
        (Term.Variable (index_of_level level bc))
        bc.gh



let top_term (bc: t): Term.t =
    term_at_level bc.sp bc


let unfilled_holes (term: Term.t) (bc: t): int list =
    Int_set.elements
        (Gamma_holes.unfilled_holes
            (count_base bc)
            term
            bc.gh)


let type_in_context (typ: Term.typ) (bc: t): type_in_context =
    unfilled_holes typ bc,
    typ,
    context bc


let required_type_in_context (bc: t): type_in_context =
    type_in_context (required_type bc) bc


let add_one_implicit
    (term: Term.t) (typ: Term.typ) (bc: t)
    : (Term.t * Term.typ * t) option
    =
    let open Term in
    match key_normal typ bc with
    | Pi (arg, res, info) when is_kind arg bc && has_variable 0 res ->
        let name = Pi_info.name info in
        let name2, implicits = Implicits.add name bc.implicits in
        Some (
            Appl (
                up1 term,
                Variable 0,
                Application_info.Implicit),
            res,
            {bc with gh =
                Gamma_holes.push_named_hole name2 arg bc.gh;
                implicits;
            }
        )
    | _ ->
        None






let add_implicits
    (term: Term.t) (typ: Term.typ) (bc: t)
    : Term.t * Term.typ * t
    =
    let rec add term typ bc =
        match add_one_implicit term typ bc with
        | None ->
            term, typ, bc
        | Some (term, typ, bc) ->
            add term typ bc
    in
    add term typ bc




let unify (act: Term.typ) (bc: t): t option =
    let req = required_type bc in
    Option.map
        (fun gh -> {bc with gh})
        (Uni.unify act req true bc.gh)



let unify_plus (term: Term.t) (bc: t): (Term.t * t) option =
    let rec uni term tp bc =
        let tp = key_normal tp bc
        and req = required_type bc in
        match Uni.unify tp req true bc.gh with
        | Some gh ->
            Some (Gamma_holes.expand term gh, {bc with gh})
        | None ->
            Option.(add_one_implicit term tp bc
                    >>= fun (term, tp, bc) ->
                    uni term tp bc)
    in
    uni term (type_of_term term bc) bc




let set_term (term: Term.t) (bc: t): t =
    {bc with
        gh =
            Gamma_holes.fill_hole (index_of_level bc.sp bc) term bc.gh}



let make (gamma: Gamma.t): t =
    let cnt0 = Gamma.count gamma in
    {
        gh =
            Gamma_holes.(
                make gamma
                |> push_hole Term.(any_uni 2)
                |> push_hole Term.(Variable 0)
            );

        base_candidates = [];

        formal_arguments = [];

        implicits = Implicits.empty;

        sp = cnt0 + 1;

        stack = [];

        entry = {
            cnt0;
        };

        entries = [];
   }




let final
    (bc: t)
    : (t * Term.t * Term.typ, int list * Term.typ * Gamma.t) result
    =
    let cnt0 = count_base bc
    and nlocs = count_entries bc in
    assert (bc.stack = []);
    assert (bc.entries = []);
    assert (bc.sp = cnt0 + 1);
    let term = term_at_level (cnt0 + 1) bc
    and typ  = term_at_level cnt0 bc
    in
    match Term.down nlocs term, Term.down nlocs typ with
    | Some term, Some typ ->
        Ok (bc, term, typ)
    | _ ->
        let gamma = Gamma_holes.context bc.gh in
        Error(
            unfilled_holes typ bc,
            typ,
            gamma
        )




let candidate
    (term: Term.t) (nargs: int) (bc: t)
    : (t, type_in_context * type_in_context) result
    =
    if 0 < nargs then
        let tp = type_of_term term bc in
        let term, tp, bc = add_implicits term tp bc in
        match unify tp bc with
        | None ->
            Error (required_type_in_context bc, type_in_context tp bc)
        | Some bc ->
            let bc = set_term term bc in
            let sp, stack = Stack.swap bc.sp bc.stack in
            Ok {bc with sp; stack}
    else
        match unify_plus term bc with
        | None ->
            Error (
                required_type_in_context bc,
                type_in_context (type_of_term term bc) bc
            )
        | Some (term, bc) ->
            Ok (set_term term bc)



let base_candidate
    (range: range)
    (variant: int)
    (term0: Term.t)
    (nargs: int)
    (bc: t)
    : t option
    =
    let term = Term.up (count_entries bc) term0 in
    match candidate term nargs bc with
    | Ok bc ->
        Some
            { bc with
                base_candidates =
                    (range, variant, term0) :: bc.base_candidates }
    | Error _ ->
        None




let find_last_ambiguous (bcs: t list): range * (Term.t * Term.typ) list =
    let split lst =
        match lst with
        | [] ->
            assert false
        | hd :: tl ->
            hd, tl
    in
    let bc, _ = split bcs
    in
    let ambiguous tops =
        let map =
            List.fold_left
                (fun map (range, variant, term) ->
                    Int_map.add variant (range,term) map)
                Int_map.empty
                tops
        in
        assert (not (Int_map.is_empty map));
        if Int_map.cardinal map = 1 then
            None
        else
            let lst = Int_map.bindings map in
            Some (
                ( match lst with
                  | [] ->
                    assert false (* cannot happen *)
                  | (_, (range, _)) :: _ ->
                    range
                ),
                List.map snd lst
            )
    in
    let rec find cands_list =
        let tops, cands_list = split cands_list in
        match ambiguous tops with
        | None ->
            find cands_list
        | Some (range, terms) ->
            let module Algo = Gamma_algo.Make (Gamma) in
            let gamma = Gamma_holes.base_context bc.gh in
            range,
            List.map
                (fun (_, term) -> term, Algo.type_of_term term gamma)
                terms
    in
    find (
        List.(
            transpose
                (map (fun bc -> bc.base_candidates) bcs))
    )




let bound
    (ibound: int) (nargs: int) (bc: t)
    : (t, type_in_context * type_in_context) result
    =
    candidate (Gamma_holes.variable_of_bound ibound bc.gh) nargs bc





let next_formal_argument
    (name: string Located.t)
    (typed: bool)
    (bc: t)
    : t
    =
    (* Register the name of the next formal argument. The hole the type of the
    argument has already been pushed. Push a new hole either for the upcoming
    formal argument after that or for the result type of the function or product
    type. *)
    let cnt0 = count bc
    and tp = top_term bc
    and str = Located.value name
    in
    {bc with
        gh =
            Gamma_holes.(
                push_bound str typed tp bc.gh
                |> push_hole Term.(any_uni 1)
            );

        formal_arguments =
            (name, cnt0) :: bc.formal_arguments;

        stack = bc.sp :: bc.stack;

        sp = cnt0 + 1
    }



let find_first_untyped_formal
    (bc: t)
    : range option
    =
    Option.map
        (fun (name,_) -> Located.range name)
        (List.find
            (fun (_, level) ->
                let open Gamma_holes in
                let typ = type_at_level level bc.gh in
                let holes = unfilled_holes 0 typ bc.gh in
                not (Int_set.is_empty holes))
            (List.rev bc.formal_arguments)
        )



let find_first_name_violation
    (bc: t)
    : (range * string * string) option
    =
    let rec find lst =
        match lst with
        | [] ->
            None
        | (name, level) :: lst ->
            let str, range = Located.value name, Located.range name
            in
            let typ = Gamma_holes.type_at_level level bc.gh
            in
            match Algo.check_naming_convention str typ bc.gh with
            | Ok () ->
                find lst
            | Error violation ->
                let case, kind = Gamma_algo.strings_of_violation violation
                in
                Some (range, case, kind)
    in
    find (List.rev bc.formal_arguments)





module Product =
(* ... A: Any1, x: A, B: Any1, y: B, ... , RT: Any1 *)
struct
    let start (bc: t): t =
        (* Push a placeholder for the first argument type. I.e. expect the first
        argument type. *)
        let cnt0 = count bc
        in
        { bc with
            gh = Gamma_holes.push_hole Term.(any_uni 1) bc.gh;

            stack = bc.sp :: bc.stack;

            sp = cnt0;

            entries = bc.entry :: bc.entries;

            entry = {cnt0}
        }

    let  check
        (nbounds: int)
        (bc: t)
        : (t, int) result
        =
        assert (0 < nbounds);
        let arg_tps, _ =
            let rec args nbounds stack tps =
                if nbounds = 0 then
                    tps, stack
                else
                    let sp, stack = Stack.split stack in
                    args (nbounds - 1) stack (term_at_level sp bc :: tps)
            in
            args nbounds bc.stack []
        in
        let rec find_incomplete i tps =
            if i = nbounds then
                None
            else
                let tp, tps = Stack.split tps in
                if Int_set.is_empty
                    (Gamma_holes.unfilled_holes bc.entry.cnt0 tp bc.gh)
                then
                    find_incomplete (i + 1) tps
                else
                    Some i
        in
        match find_incomplete 0 arg_tps with
        | Some i ->
            Error i
        | None ->
            Ok bc


    let end_
        (nargs: int)
        (nbounds: int)
        (bc: t)
        : (t, type_in_context * type_in_context) result
        =
        assert (0 < nbounds);
        let res = top_term bc
        and sp, stack = Stack.pop (nbounds + 1) bc.sp bc.stack
        in
        let tp = Gamma_holes.pi nbounds res bc.gh in
        let entry, entries = Stack.split bc.entries in
        candidate
            tp
            nargs
            { bc with
                gh = Gamma_holes.remove_bounds nbounds bc.gh;
                sp;
                stack;
                entry;
                entries;
            }
end






module Typed =
(* [exp : tp] *)
struct
    let start (bc: t): t =
        (* Expect the type term. *)
        let cnt0 = count bc in
        {bc with
            gh = Gamma_holes.push_hole Term.(any_uni 1) bc.gh;

            stack = bc.sp :: bc.stack;

            sp = cnt0
        }

    let expression (bc: t): t =
        (* Expect the expression. *)
        let cnt0 = count bc in
        {bc with
            gh = Gamma_holes.push_hole (top_term bc) bc.gh;

            stack = bc.sp :: bc.stack;

            sp = cnt0;
        }

    let end_
        (nargs: int) (bc: t)
        : (t, type_in_context * type_in_context) result
        =
        let tp, stack = Stack.split bc.stack in
        let sp, stack = Stack.split stack in
        let tp = term_at_level tp bc
        and exp = top_term bc
        in
        let term = Term.Typed (exp, tp)
        and bc = {bc with sp; stack} in
        candidate term nargs bc
end






module Application =
(* [f a b c ...]

    f   a   b   c
    . g .
    .    h  .


For each argument we introduce 4 holes:

    A: Any(1)
    F: A -> Any(1)
    a: A
    f: all (x: A): F x
    ...

    stack = f a g b h c ...

*)
struct
    let start (nargs: int) (bc: t): t =
        let cnt0 = count bc in
        let rec shift cnt0 n gh sp stack =
            if n = 0 then
                gh, sp, stack
            else
                let open Gamma_holes in
                shift
                    (cnt0 + 4)
                    (n - 1)
                    (push_hole Term.(any_uni 1) gh  (* A: Any(1) *)
                    |> push_hole                    (* F: A -> Any(1) *)
                        Term.(Pi (Variable 0, any_uni 1, Pi_info.arrow))
                    |> push_hole Term.(Variable 1)  (* a: A *)
                    |> push_hole Term.(             (* f: all (x:A): F x *)
                        Pi (Variable 2,
                            Appl (
                                Variable 2,
                                Variable 0,
                                Application_info.Normal),
                            Pi_info.typed "x")))
                    (cnt0 + 3)
                    (cnt0 + 2 :: sp :: stack)
        in
        let gh, sp, stack = shift cnt0 nargs bc.gh bc.sp bc.stack in
        {bc with
            gh;
            stack;
            sp}


    let apply
        (n_remaining: int)
        (mode: Term.Application_info.t)
        (bc: t)
        : (t, type_in_context * type_in_context) result
        =
        match bc.stack with
        | f :: e :: stack ->
            let open Term in
            let term = Appl (
                    term_at_level f bc,
                    term_at_level bc.sp bc,
                    mode)
            in
            candidate
                term
                n_remaining
                {bc with
                    stack;
                    sp = e}
        | _ ->
            assert false (* Illegal call! *)
end (* Application *)





module Lambda =
(*
    \ (x: A) (y: B) ... : T := t

One placeholder for each optional argument type, one placeholder for the
optional result type and one placeholder for the inner expression.

    stack = t T ... B A ...
*)
struct
    let start (bc: t): t =
    (* Push a placeholder for the first argument type. I.e. expect the first
    argument type. *)
        let cnt0 = count bc in
        {bc with
            gh = Gamma_holes.push_hole Term.(any_uni 1) bc.gh;

            stack = bc.sp :: bc.stack;

            sp = cnt0
        }


    let inner (bc: t): t =
    (* Push a placeholder based on the result type (the previously analyzed ast),
    i.e. expect the inner term of the function abstraction. *)
        let cnt0 = count bc
        and tp = top_term bc
        in
        let open Gamma_holes in
        {bc with
            gh = push_hole tp bc.gh;

            stack = bc.sp :: bc.stack;

            sp = cnt0
        }


    let end_
        (nargs: int) (nbounds: int) (typed: bool) (bc: t)
        : (t, type_in_context * type_in_context) result
        =
        let exp = top_term bc in
        let sp, stack = Stack.split bc.stack in
        let exp =
            if typed then
                Term.Typed (exp, term_at_level sp bc)
            else
                exp
        in
        let lam = Gamma_holes.lambda nbounds exp bc.gh in
        let sp, stack = Stack.pop (nbounds + 1) sp stack
        in
        candidate
            lam
            nargs
            {bc with
                gh = Gamma_holes.remove_bounds nbounds bc.gh;
                sp;
                stack
            }
end





module Where =
struct
    let start (name: string Located.t) (bc: t): t =
        Application.start 1 bc
        |> Lambda.start
        |> next_formal_argument name false
        |> Lambda.inner



    let end_inner (bc: t): (t, type_in_context * type_in_context) result =
        Lambda.end_ 1 1 false bc


    let end_
        (nargs: int)
        (bc: t)
        : (t, type_in_context * type_in_context) result
        =
        match bc.stack with
        | f :: e :: stack ->
            let open Term in
            (
                match term_at_level f bc with
                | Lambda (tp, exp, info) ->
                    let term =
                        Where (
                            Lambda_info.name info,
                            tp,
                            exp,
                            term_at_level bc.sp bc
                        )
                    in
                    candidate term nargs {bc with stack; sp = e}
                | _ ->
                    assert false (* Illegal call! *)
            )
        | _ ->
            assert false (* Illegal call! *)
end
