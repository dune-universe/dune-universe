open Fmlib
open Common



type t = {
    gamma: Gamma.t;
    map: Name_map.t
  }


let empty: t = {
        gamma = Gamma.empty;
        map   = Name_map.empty;
}


let count (c: t): int =
    Gamma.count c.gamma



let gamma (c: t): Gamma.t =
    c.gamma



let name_map (c: t): Name_map.t =
    c.map


let index_of_level (level: int) (c: t): int =
    Gamma.index_of_level level c.gamma

let level_of_index = index_of_level



let add_new_globals (gamma: Gamma.t) (c: t): t =
    let cnt0 = count c in
    assert (cnt0 <= Gamma.count gamma);
    {
        gamma;

        map =
        Interval.fold
            c.map
            (fun i m ->
                let open Gamma in
                match
                    Name_map.add_global
                        (name_at_level i gamma)
                        (type_at_level i gamma)
                        gamma
                        m
                with
                | Error _ ->
                    Printf.printf "Context.standard Cannot add %s\n"
                        (name_at_level i gamma);
                    assert false
                | Ok map ->
                    map
            )
            cnt0
            (Gamma.count gamma)
    }



let compute (t: Term.t) (c: t): Term.t =
    Gamma.compute t c.gamma


let find_name (name: string) (c: t): int list =
    Name_map.find name c.map



let push_local (name: string) (typ: Term.typ) (c: t): t =
    {
        gamma =
            Gamma.push_local name typ c.gamma;

        map =
            Name_map.add_local name c.map;
    }



let can_add_global (name: string) (typ: Term.typ) (c: t): bool =
    match
        Name_map.add_global name typ c.gamma c.map
    with
    | Ok _ ->
        true
    | Error _ ->
        false



let add_axiom (name: string) (typ: Term.typ) (c: t): t =
    {
        gamma =
            Gamma.add_axiom name typ c.gamma;

        map =
            Name_map.add_global_strict name typ c.gamma c.map;
    }




let add_builtin_type
    (descr: string)
    (name: string)
    (typ: Term.typ)
    (c: t)
    : t
=
    {
        gamma =
            Gamma.add_builtin_type descr name typ c.gamma;

        map =
            Name_map.add_global_strict name typ c.gamma c.map;
    }



let add_builtin_function
    (descr: string)
    (name: string)
    (typ: Term.typ)
    (c: t)
    : t
=
    {
        gamma =
            Gamma.add_builtin_function descr name typ c.gamma;

        map =
            Name_map.add_global_strict name typ c.gamma c.map;
    }




let add_definition
    (name: string) (typ: Term.typ) (exp: Term.t) (c: t)
    : (t, int) result
=
    Result.map
        (fun map ->
            {
                map;

                gamma =
                    Gamma.add_definition name typ exp c.gamma
            })
        (Name_map.add_global
            name
            typ
            c.gamma
            c.map)



let add_inductive (ind: Inductive.t) (c: t): t =
    add_new_globals
        (Gamma.add_inductive ind c.gamma)
        c





module Pretty (P: Pretty_printer.SIG) =
struct
    module P0 = Term_printer.Pretty (Gamma) (P)

    let print (t:Term.t) (c:t): P.t =
        P0.print t c.gamma
end
