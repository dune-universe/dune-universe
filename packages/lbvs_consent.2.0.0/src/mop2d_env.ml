(* molprint2D atom environment with max distance = 2 bonds *)

open Printf

module L = MyList

(*       center,  direct_neighbors,     undirect_neighbors *)
type t = string * (string * int) list * (string * int) list

let to_string ((center, neighbors, neighbors'): t): string =
  Printf.sprintf "%s-%s-%s"
    center
    (L.to_string (fun (typ, count) -> sprintf "(%s,%d)" typ count) neighbors)
    (L.to_string (fun (typ, count) -> sprintf "(%s,%d)" typ count) neighbors')

let of_string (s: string): t =
  let center, neighbors, neighbors' =
    try Scanf.sscanf s "%s@-%s@-%s" (fun a b c -> a, b, c)
    with _ -> failwith ("Mop2d_env.of_string: cannot parse triplet: " ^ s)
  in
  let of_pair_str s =
    try Scanf.sscanf s "(%s@,%d)" (fun a b -> (a, b))
    with _ -> failwith ("Mop2d_env.of_string: cannot parse pair: " ^ s)
  in
  (center,
   L.of_string of_pair_str neighbors,
   L.of_string of_pair_str neighbors')
