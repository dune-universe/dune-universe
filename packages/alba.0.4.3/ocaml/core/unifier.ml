(*

Unification
-----------


    We have a context with holes gh and two welltyped terms t1 and t2. The
    unification problem is expressed as on of

        gh |- le! t1 t2

        gh |- eq! t1 t2

    requiring to fill the holes in gh such that the substituted terms satisfy

        gh |- t1 <= t2

        gh |- t1 =  t2

    where `=` means beta equality.

    The unification fails, if no substitution can be found for the holes (or
    metavariables) in gh.

    If the terms t1 ant t2 contain no holes, unification is possible only if
    both expressions satisfy the required relation. This implies that the normal
    forms both terms have the same structure.



Algorithm
---------

    The algorithm is a recursive procedure which recurses over the normal form
    of both terms. In each step we transform the two terms u and v into the key
    normal forms

        t1 = k1 a0 a1 ...
        t2 = k2 b0 b1 ...

    The key normal form is always one of

        s
        lambda x:A . e
        Pi x:A . B
        x a0 a1 ...

    i.e. arguments can only be present if the key is a variable.

    Since we might enter subexpressions with binders, the context might consist
    of

        gh, psi

    where psi represents the entered binders. psi does not contain any holes.

    The interesting cases are when one of the keys or both keys are holes. In
    all other cases futhere unification is possible only if both expressions
    have the same structure.



Filling of holes
----------------

    Replace with lambda term (application to local variables only):

        Suppose we have to unify

            gh,psi |- rel! (f x y ...) t

        where f is a hole and x y ... are variables in psi (might be a subset).

        This is possible only if

            gh |- f: all (x: A) (y: B) ... : R

        is valid. We can fill the hole with

            f := \ x y ... := t

        if the following conditions are satisfied:

            - If t contains variables of psi, then they all belong to the set x
              y ...

            - type (\x y ... := t) <= type f


    Mirror image `gh,psi |- rel! t (f x y ...)` is practically the same:

    Both are holes (local variables only):

        Suppose we have to unify

            gh,psi |- rel! (f x y ...) (g x y ...)

        where f and g are holes and x y ... are variables in psi (might be
        subset).

            f := g, if gh |- type g <= type f

            g := f, if gh |- type f <= type g

        If either f or g is applied to fewer local variables than the other, we
        have to apply the first case to the hole which is applied to more
        variables (case is no longer symmetrical).

    Both are holes (local variables only) with different order of variables

        Suppose we have to unify

            gh,psi |- rel! (f x y ...) (g y x ...)

        where f and g are holes and x y ... are variables in psi (possibly
        subset).

            f := \ x y ... := g y x ..., if type (\ x y ...) <= type f

            g := \ y x ... := f x y ..., if type (\ y x ...) <= type g


    Replace with variable (general application):

        Suppose we have to unify

            gh,psi |- rel! (f a0 a1 ...) (x b0 b1 ...)

        where f is a hole and x is not a hole.

            f := x, if  type x <= type f

        and then unify eq! a0 b0, eq! a1 b2. The condition `type x <= type f`
        implies that the number of arguments on both sides is the same.



Substitution Decision
---------------------

    Situtations:

    - key is a hole, i.e. on term has the form

        f x y ...


    - lambda condition satisfied: {x,y,...} subset |psi| and the other term
      contains not more than {x,y,...} from psi.


    Interesting cases:

    - One key is a hole and satisfies the lambda condition and the other is not
      a hole or does not satisfy the lambda condition

      -> do lambda, if possible or fail

    - Both keys are holes satisfying the lambda condition. Then distinguish

        - arguments are identical

            -> substitute subtype for the other or fail if none of the types is
               a subtype of the other

        - arguments are different

            -> do lambda the way it is possible or fail.


    In all other cases we have to do structural unification.



Generalisation
--------------

    Whenever we have to unify

        rel! (f a b c ...)  t

    we can try the substitution

        f := (\ x y ... := t)

    provided that

        type (\ x y ... := t) <= type f

    and t does not contain more local variables than [f a b c ...] and a b c ...
    are either local variables or do not contain local variables.

*)




open Fmlib
open Common


module type HOLES =
sig
    include Gamma_algo.GAMMA

    val context: t -> Gamma.t

    val expand: Term.t -> t -> Term.t

    val is_hole: int -> t -> bool

    val value: int -> t -> Term.t option

    val fill_hole0: int -> Term.t -> bool -> t -> t

    val fold_entries:
        (int -> int -> string -> Term.typ -> bool -> Term.t option -> 'a -> 'a)
        -> t
        -> 'a
        -> 'a
end




module Unification_context (Gh: HOLES) =
struct
    type t = {
        gh: Gh.t;
        gamma_top: Gamma.t;
        stack: Gamma.t list;
    }


    let base (uc: t): Gh.t =
        uc.gh

    let gamma (uc: t): Gamma.t =
        uc.gamma_top


    let nlocals (uc: t): int =
        Gamma.count uc.gamma_top - Gh.count uc.gh


    let count (uc: t): int =
        Gamma.count uc.gamma_top


    let is_valid_index (idx: int) (uc: t): bool =
        idx < count uc


    let name_of_index (idx: int) (uc: t): string =
        assert (is_valid_index idx uc);
        Gamma.name_of_index idx uc.gamma_top


    let definition_term (idx: int) (uc: t): Term.t option =
        assert (is_valid_index idx uc);
        Gamma.definition_term idx uc.gamma_top


    let type_of_literal (lit: Term.Value.t) (uc: t): Term.typ =
        Gamma.type_of_literal lit uc.gamma_top


    let is_hole (idx: int) (uc: t): bool =
        let nlocs = nlocals uc in
        if idx < nlocs then
            false
        else
            Gh.is_hole (idx - nlocs) uc.gh


    let is_empty_hole (idx: int) (uc: t): bool =
        let nlocs = nlocals uc
        in
        if idx < nlocs then
            false
        else
            Gh.value (idx - nlocs) uc.gh = None


    let fill_hole
        (idx: int)
        (typ: Term.typ)
        (uc: t)
        : t option
        =
        assert (is_hole idx uc);
        let nlocs = nlocals uc
        in
        Option.map
            (fun typ0 ->
                {uc with
                    gh =
                        Gh.fill_hole0
                            (idx - nlocs)
                            typ0
                            true
                            uc.gh})
            (Term.down nlocs typ)


    let expand (term: Term.t) (uc: t): Term.t =
        let nlocs = nlocals uc in
        Term.substitute_with_beta
            (fun i ->
                if i < nlocs then
                    Variable i
                else
                    match Gh.value (i - nlocs) uc.gh with
                    | None ->
                        Variable i
                    | Some term ->
                        Term.up nlocs term)
            term


    let type_of_variable (idx: int) (uc: t): Term.typ =
        assert (is_valid_index idx uc);
        let nlocs = nlocals uc
        in
        if idx < nlocs then
            (*expand (Gamma.type_of_variable idx uc.gamma_top) uc*)
            Gamma.type_of_variable idx uc.gamma_top
        else
            Term.up
                nlocs
                (Gh.type_of_variable (idx - nlocs) uc.gh)


    let string_of_term (term: Term.t) (uc: t): string =
        Term_printer.string_of_term term uc.gamma_top

    let _ = string_of_term


    let make (gh: Gh.t): t =
        { gh; gamma_top = Gh.context gh; stack = [] }


    let push (name: string) (typ: Term.typ) (uc: t): t =
        { uc with
            gamma_top =
                Gamma.push_local name typ uc.gamma_top;

            stack =
                uc.gamma_top :: uc.stack;
        }


    let push_local = push


    let pop (uc: t): t =
        match uc.stack with
        | [] ->
            assert false (* Illegal call! *)

        | gamma_top :: stack ->
            {uc with gamma_top; stack}

end (* Unification_context *)






module Make (Gh: HOLES) =
struct
    module Uc = Unification_context (Gh)
    module Algo = Gamma_algo.Make (Uc)

    type path_step = Term.t * Term.t * bool * Gamma.t

    type path = path_step list

    type args = (Term.t * Term.Application_info.t) array

    type 'a result2 = ('a, path * Gh.t) result

    module Result2 = Result.Make (struct type t = path * Gh.t end)



    let key_split
        (term: Term.typ) (uc: Uc.t):
        Term.t * args
    =
        let key, args = Algo.key_split (Uc.expand term uc) uc
        in
        key, Array.of_list args




    let unify_sorts
        (act_sort: Term.Sort.t)
        (req_sort: Term.Sort.t)
        (is_super: bool)
        (path: path) (uc: Uc.t):
        Uc.t result2
    =
        if
            (is_super && Term.Sort.is_super req_sort act_sort)
            ||
            (not is_super && req_sort = act_sort)
        then
            Ok uc
        else
            Error (path, Uc.base uc)


    let make_lambda
        (idx_hole: int)
        (args: args)
        (term: Term.t)
        (path: path)
        (uc: Uc.t):
        Term.t result2
    =
        let nlocs = Uc.nlocals uc
        and nargs = Array.length args
        and error _ = Error (path, Uc.base uc)
        and idx_map = ref Int_map.empty
        in
        let module TMon = Term.Monadic (Result2) in
        let transform iarg =
            TMon.map_free
                (fun i ->
                    if i = idx_hole then
                        error ()
                    else
                        match Int_map.maybe_find i !idx_map with
                        | None ->
                            if i < nlocs then
                                error ()
                            else
                                Ok (i + iarg)
                        | Some i_new ->
                            Ok i_new
                )
        in
        let rec make iarg =
            let open Term in
            if iarg = nargs then
                transform nargs term
            else
                let open Result in
                match args.(iarg) with
                | Variable i, _  when not (Int_map.mem i !idx_map) ->
                    idx_map :=
                        Int_map.add
                            i
                            (bruijn_convert iarg nargs)
                            !idx_map;
                    make (iarg + 1) >>= fun exp ->
                    transform iarg (Uc.type_of_variable i uc)
                    >>= fun arg_typ ->
                    Ok (lambda (Uc.name_of_index i uc) arg_typ exp)
                | arg, _ ->
                    make (iarg + 1) >>= fun exp ->
                    transform iarg (Algo.type_of_term arg uc)
                    >>= fun arg_typ ->
                    Ok (lambda "_" arg_typ exp)
        in
        make 0





    let rec unify0
        (act: Term.t) (req: Term.t) (is_super: bool)
        (path: path)
        (uc: Uc.t):
        Uc.t result2
    =
        let act_key, act_args = key_split act uc
        and path = (act, req, is_super, (Uc.gamma uc)) :: path
        in
        let open Term
        in
        match act_key with
        | Typed _ | Appl _ | Where (_, _, _, _) ->
            assert false (* Cannot happen in key splitted form. *)

        | Sort act_sort ->
            assert (Array.is_empty act_args);
            let req_key, req_args = key_split req uc in
            begin match req_key with
                | Typed _ | Appl _ | Where (_, _, _, _) ->
                    assert false (* Cannot happen in key splitted form. *)

                | Value _ | Lambda _ | Pi _ ->
                    Error (path, Uc.base uc)

                | Variable j when not (Uc.is_hole j uc) ->
                    Error (path, Uc.base uc)

                | Sort req_sort ->
                    assert (Array.is_empty req_args);
                    unify_sorts act_sort req_sort is_super path uc

                | Variable j ->
                    assert (Uc.is_hole j uc);
                    fill_hole j req_args act path uc
            end

        | Value act_value ->
            assert (Array.is_empty act_args);
            let req_key, req_args = key_split req uc in
            (
                match req_key with
                | Typed _ | Appl _ | Where (_, _, _, _) ->
                    assert false (* Cannot happen in key splitted form. *)

                | Value req_value when act_value = req_value ->
                    assert (Array.is_empty req_args);
                    Ok uc

                | Variable j when Uc.is_hole j uc ->
                    assert (Uc.is_empty_hole j uc);
                    fill_hole j req_args act path uc

                | _ ->
                    Error (path, Uc.base uc)
            )

        | Variable i ->
            let req_key, req_args = key_split req uc in
            (
                match req_key with
                | Typed _ | Appl _ | Where (_, _, _, _) ->
                    assert false (* Cannot happen in key splitted form. *)

                | Variable j ->
                   unify_variable_variable
                    i act_args act
                    j req_args req
                    path uc

                | _ ->
                    if Uc.is_hole i uc then
                        fill_hole i act_args req path uc
                    else
                        Error (path, Uc.base uc)
            )

        | Lambda (_, _, _) ->
            assert (Array.is_empty act_args);
            assert false (* nyi *)

        | Pi (act_arg, act_res, info) ->
            assert (Array.is_empty act_args);
            (
                let req_key, req_args = key_split req uc in
                match req_key with
                | Variable j when Uc.is_hole j uc ->
                    assert (Uc.is_empty_hole j uc);
                    fill_hole j req_args act path uc

                | Pi (req_arg, req_res, _) ->
                    let open Result in
                    unify0 act_arg req_arg false path uc
                    >>=
                    unify_pushed
                        (Pi_info.name info)
                        act_arg
                        act_res
                        req_res
                        true
                        path

                | _ ->
                    Error (path, Uc.base uc)
            )


    and unify_variable_variable
        (i: int) (iargs: args) (iterm: Term.t)
        (j: int) (jargs: args) (jterm: Term.t)
        (path: path) (uc: Uc.t):
        Uc.t result2
    =
        if i = j then
            begin
                let nargs = Array.length iargs in
                assert (nargs = Array.length jargs);
                let module IntMon = Interval.Monadic (Result2) in
                IntMon.fold
                    (fun k ->
                        unify0
                            (fst iargs.(k))
                            (fst jargs.(k))
                            false
                            path
                    )
                    0 nargs
                    uc
            end
        else
            let ihole = Uc.is_hole i uc
            and jhole = Uc.is_hole j uc
            in
            if ihole && jhole then
                Result2.catch
                    (fill_hole i iargs jterm path uc)
                    (fun _ -> fill_hole j jargs iterm path uc)

            else if ihole then
                fill_hole i iargs jterm path uc

            else if jhole then
                fill_hole j jargs iterm path uc

            else
                Error (path, Uc.base uc)


    and unify_pushed
        (name: string) (typ: Term.typ)
        (act: Term.t) (req: Term.t) (is_super: bool)
        (path: path) (uc: Uc.t):
        Uc.t result2
    =
        Result.map
            Uc.pop
            (unify0 act req is_super path (Uc.push name typ uc))


    and fill_hole
        (idx: int)
        (args: args)
        (term: Term.t)
        (path: path)
        (uc: Uc.t):
        Uc.t result2
    =
        (* unify (idx args) term,

            where [idx] is a hole and [term] is not starting with a hole. The
            lambda condition has not yet been checked.
        *)
        assert (Uc.is_empty_hole idx uc);
        let open Result2 in
        make_lambda idx args term path uc
        >>= fun lambda ->
        fill_simple_hole idx lambda path uc


    and fill_simple_hole
        (idx: int) (term: Term.t)
        (path: path) (uc: Uc.t):
        Uc.t result2
    =
        assert (Uc.is_empty_hole idx uc);
        let typ_hole = Uc.type_of_variable idx uc
        and typ_term = Algo.type_of_term term uc
        in
        Result.(
            unify0 typ_term typ_hole true path uc
            >>= fun uc ->
            match
                Uc.fill_hole idx term uc
            with
            | None ->
                Printf.printf "  shall never happen\n";
                Error (path, Uc.base uc)
            | Some uc ->
                Ok uc
        )











    let print_holes (gh: Gh.t): unit =
        let string_of term =
            Term_printer.string_of_term term (Gh.context gh)
        in
        Gh.fold_entries
            (fun _ _ name typ is_hole value () ->
                let open Printf
                in
                if is_hole then
                    match value with
                    | None ->
                        printf "  %s: %s\n"
                            name (string_of typ)
                    | Some value ->
                        printf "  %s: %s := %s\n"
                            name (string_of typ) (string_of value)
                else
                    printf "  %s: %s\n"
                        name (string_of typ)
            )
            gh
            ()
    let _ = print_holes



    let unify
        (act: Term.typ) (req: Term.typ) (is_super: bool) (gh: Gh.t):
        Gh.t option
    =
        match unify0 act req is_super [] (Uc.make gh) with
        | Ok uc ->
            (*let open Printf in
            let open Term_printer in
            let gamma = Gh.context gh in
            printf "\nsuccessful unification %s with %s\n"
                (string_of_term act gamma)
                (string_of_term req gamma);
            printf "holes\n";
            print_holes gh;
            printf "\n";*)
            Some (Uc.base uc)

        | Error (_, _) ->
            (*let open Printf in
            let print ((act,req,is_super,gamma)) =
                let open Term_printer in
                printf "  %s with %s (%b)\n"
                    (string_of_term act gamma)
                    (string_of_term req gamma)
                    is_super
            in
            printf "\nfailed unification\n";
            List.iter print path;
            printf "holes\n";
            print_holes gh;
            printf "\n";*)
            None
end (* Make *)
