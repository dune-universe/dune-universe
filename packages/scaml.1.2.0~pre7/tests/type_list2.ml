[@@@SCaml iml_optimization=false]
open SCaml
(* Check it is compiled to PUSH (list string) { "a"; "b"; "c" } *)
let [@entry] main () () = 
  [], 
  let a = "a" in
  assert (List.length [a; "b"; "c"] = Nat 3)
