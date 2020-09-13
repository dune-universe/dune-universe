(*
   STORAGE= (Map [] : (nat, string) map)
*)
open SCaml

let [@entry] main () _ =
  [],
  List.fold_left (fun acc (k,v) -> Map.update k (Some v) acc) Map.empty 
    [ (Nat 0, "hello")
    ; (Nat 1, "world")
    ; (Nat 2, "x")
    ; (Nat 3, "y")
    ]
