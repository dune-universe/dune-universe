open Fmlib
open Common


type entry =
    | Global of int Term_trie.t * int list
    | Local of int


type t =
    {map: entry String_map.t; has_locs: bool; cnt: int}


let empty: t =
    {map = String_map.empty; has_locs = false; cnt = 0}


let count (m:t): int =
    m.cnt


let has_locals (m: t): bool =
    m.has_locs




let find (name:string) (m:t): int list =
    match String_map.maybe_find name m.map with
    | None ->
        []
    | Some e ->
        match e with
        | Global (_, lst) ->
            lst
        | Local i ->
            [i]



let add_unnamed (m: t): t =
    { m with
        cnt = 1 + m.cnt;
    }



let add_to_trie
    (typ: Term.typ)
    (gamma: Gamma.t)
    (trie: int Term_trie.t)
    : (int Term_trie.t, int) result
    =
    let cnt = Gamma.count gamma
    in
    Term_trie.add_new typ cnt cnt trie



let add_global
    (name: string)
    (typ: Term.typ)
    (gamma: Gamma.t)
    (m: t)
    : (t, int) result
    =
    assert (name <> "");
    assert (not (has_locals m));
    if name = "_" then
        Ok (add_unnamed m)
    else
        let module Algo = Gamma_algo.Make (Gamma)
        in
        let typ =
            Algo.normalize typ gamma
        in
        Result.(map
            (fun value ->
                {m with
                    map = String_map.add name value m.map;
                    cnt = m.cnt + 1})
            (
                match String_map.maybe_find name m.map with
                | None ->
                    map
                        (fun trie ->
                            Global (trie, [m.cnt]))
                        (add_to_trie typ gamma Term_trie.empty)

                | Some (Global (trie, lst)) ->
                    map
                        (fun trie ->
                            Global (trie, m.cnt :: lst))
                        (add_to_trie typ gamma trie)

                | Some (Local _) ->
                    assert false (* Cannot happen, there are no locals. *)
            )
        )


let add_global_strict
    (name: string)
    (typ: Term.typ)
    (gamma: Gamma.t)
    (m: t)
    : t
=
    match add_global name typ gamma m with
    | Ok m ->
        m
    | _ ->
        assert false (* Illegal call! *)



let add_local (name: string) (m: t) : t =
    assert (name <> "");
    if name = "_" then
        add_unnamed m
    else
    {
        map = String_map.add name (Local m.cnt) m.map;

        has_locs = true;

        cnt = 1 + m.cnt;
    }
