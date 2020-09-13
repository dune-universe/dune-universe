(*
   STORAGE= Int 2
   INPUT= ()
*)
[@@@SCaml iml_optimization=false]
open SCaml
let [@entry] main x (storage : int) = 
  [],
  Loop.left (fun x -> if x = Int 0 then Right storage else Left (x - Int 1)) (Int 10)
