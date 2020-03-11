open Fmlib
open Common



module Name_map =
  struct
    type t =
      {map: int list String_map.t; cnt: int}

    let empty: t =
      {map = String_map.empty; cnt = 0}

    let count (m:t): int =
      m.cnt

    let find (name:string) (m:t): int list =
      match String_map.maybe_find name m.map with
      | None ->
         []
      | Some lst ->
         lst


    let add_unnamed (m:t): t =
      {m with cnt = 1 + m.cnt}

    let add (name:string) (global:bool) (m:t): t =
      assert (name <> "");
      if name = "_" then
        add_unnamed m
      else
        {map =
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
         cnt =
           1 + m.cnt}

    let add_global (name:string) (m:t): t =
      assert (name <> "");
      add name true m

    let add_local (name: string) (m:t) : t =
      assert (name <> "");
      add name false m
  end


type t = {
    gamma: Gamma.t;
    map: Name_map.t
  }



let count (c:t): int =
  Gamma.count c.gamma


let gamma (c:t): Gamma.t =
  c.gamma

let name_map (c:t): Name_map.t =
  c.map


let standard (): t =
  let gamma = Gamma.standard () in
  {gamma;
   map =
     Interval.fold
       Name_map.empty
       (fun i m ->
         Name_map.add_global
           Gamma.(name_at_level i gamma)
           m)
       0 (Gamma.count gamma)
  }


let compute (t:Term.t) (c:t): Term.t =
  Gamma.compute t c.gamma


let find_name (name:string) (c:t): int list =
  Name_map.find name c.map


module Pretty (P:Pretty_printer.SIG) =
  struct
    module P0 = Term_printer.Pretty (Gamma) (P)
    let print (t:Term.t) (c:t): P.t =
      P0.print t c.gamma
  end
