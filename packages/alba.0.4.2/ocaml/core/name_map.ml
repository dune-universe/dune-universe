open Fmlib
open Common



type t =
    {map: int list String_map.t; has_locs: bool; cnt: int}

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
    | Some lst ->
        lst


let add_unnamed (m:t): t =
    {m with cnt = 1 + m.cnt; has_locs = true}


let add (name:string) (global:bool) (m:t): t =
    assert (name <> "");
    if name = "_" then
        add_unnamed m
    else
        {
            map =
                String_map.add
                 name
                 (match String_map.maybe_find name m.map with
                  | None ->
                     [m.cnt]
                  | Some lst ->
                     if global then
                       m.cnt :: lst
                     else
                       [m.cnt])
                 m.map;
            has_locs =
                not global;
            cnt =
                1 + m.cnt
        }

let add_global (name:string) (m:t): t =
    assert (name <> "");
    assert (not (has_locals m));
    add name true m

let add_local (name: string) (m:t) : t =
    assert (name <> "");
    add name false m
