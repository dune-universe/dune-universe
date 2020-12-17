open Fmlib

module Int_map   = Common.Int_map
module Sort_map  = Finite_map.Make (Sort)
module Value_map = Finite_map.Make (Value)


(*

Term_trie
---------

The term

    all (A: Any) (a: A) (B: Any) (f: A -> B): B

in its long form

    all (A: Any): all (a: A): all (B: Any): all (f: all (_: A): B): B


has the preorder traversal

    all Any all A all Any all all A B B


If the term is in the trie, then there must be a corresponding path from the
root.


*)





type 'a t = {
    sorts:    ('a base) Sort_map.t;
    bounds:   ('a base) Int_map.t;
    globals:  ('a base) Int_map.t;
    applications: 'a t option;
    typeds:   'a t option;
    lambdas:  'a t option;
    products: 'a t option;
}
and 'a base =
    | End of 'a
    | Next of 'a t




let empty: 'a t = {
        sorts    = Sort_map.empty;
        bounds   = Int_map.empty;
        globals  = Int_map.empty;
        applications  = None;
        typeds   = None;
        lambdas  = None;
        products = None;
    }






type 'a find_cont = 'a t -> 'a option

let find (term: Term.t) (n: int) (trie: 'a t): 'a option =
    let open Option
    in
    let find_base k base =
        base >>= fun base ->
        match base, k with
        | End a, None ->
            Some a
        | Next next, Some k ->
            k next
        | _, _ ->
            assert false (* Illegal call! *)
    in
    let rec find term nb (k: 'a find_cont option) trie: 'a option =
        let open Term in
        match term with
        | Value _ ->
            assert false (* nyi *)

        | Sort s ->
            find_base
                k
                (Sort_map.maybe_find s trie.sorts)

        | Variable i when i < nb ->
            find_base
                k
                (Int_map.maybe_find i trie.bounds)

        | Variable i ->
            find_base
                k
                (Int_map.maybe_find
                    (Term.bruijn_convert (i - nb) n)
                    trie.globals)

        | Typed (exp, tp) ->
            trie.typeds
            >>=
            find exp nb (Some (find tp nb k))


        | Appl (f, arg, _ ) ->
            trie.applications
            >>=
            find f nb (Some (find arg nb k))

        | Lambda (tp, exp, _ ) ->
            trie.lambdas
            >>=
            find tp nb (Some (find exp (nb + 1) k))


        | Pi (tp, res, _) ->
            trie.products
            >>=
            find tp nb (Some (find res (nb + 1) k))

        | Where (_, _, _, _) ->
            assert false (* nyi *)
    in
    find term 0 None trie






let map_result (f: 'a -> 'b): ('a, 'e) result -> ('b, 'e) result =
    function
    | Ok a ->
        Ok (f a)
    | Error e ->
        Error e


type 'a add_cont = 'a t -> ('a t, 'a) result



let add_base
    (a: 'a)
    (k: 'a add_cont option)
    (add: 'a base -> 'a t)
    (base: 'a base option)
    : ('a t, 'a) result
    =
    match base with
    | None ->
        (
            match k with
            | None ->
                Ok (add (End a))
            | Some k ->
                map_result
                    (fun next -> add (Next next))
                    (k empty)
        )
    | Some base ->
        match base, k with
        | End a, None ->
            Error a
        | Next next, Some k ->
            k next
        | _, _ ->
            assert false (* Illegal call! *)




let add_compound
    (add_outer: 'a t -> 'a t)
    (add_inner: 'a t -> ('a t, 'a) result)
    (inner: 'a t option)
    : ('a t, 'a) result
    =
    map_result
        add_outer
        (
            match inner with
            | None ->
                add_inner empty
            | Some inner ->
                add_inner inner
        )





let add_new (term: Term.t) (n: int) (a: 'a) (trie: 'a t): ('a t, 'a) result =
    let rec add term nb k trie =
        let open Term in
        match term with
        | Value _ ->
            assert false (* nyi *)

        | Sort s ->
            add_base
                a
                k
                (fun value ->
                    {trie with
                        sorts =
                            Sort_map.add s value trie.sorts})
                (Sort_map.maybe_find s trie.sorts)

        | Variable i when i < nb ->
            add_base
                a
                k
                (fun value ->
                    {trie with
                        bounds =
                            Int_map.add i value trie.bounds})
                (Int_map.maybe_find i trie.bounds)

        | Variable i ->
            let level = Term.bruijn_convert (i - nb) n
            in
            add_base
                a
                k
                (fun value ->
                    {trie with
                        globals =
                            Int_map.add level value trie.globals})
                (Int_map.maybe_find level trie.globals)

        | Typed (exp, tp) ->
            add_compound
                (fun res -> {trie with typeds = Some res})
                (add exp nb (Some (add tp nb k)))
                trie.typeds


        | Appl (f, arg, _ ) ->
            add_compound
                (fun res -> {trie with applications = Some res})
                (add f nb (Some (add arg nb k)))
                trie.applications

        | Lambda (tp, exp, _ ) ->
            add_compound
                (fun res -> {trie with lambdas = Some res})
                (add tp nb (Some (add exp (nb + 1) k)))
                trie.lambdas


        | Pi (tp, res, _) ->
            add_compound
                (fun res -> {trie with products = Some res})
                (add tp nb (Some (add res (nb + 1) k)))
                trie.products

        | Where (_, _, _, _) ->
            assert false (* nyi *)
    in
    add term 0 None trie
