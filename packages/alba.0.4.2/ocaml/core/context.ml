open Fmlib
open Common



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
