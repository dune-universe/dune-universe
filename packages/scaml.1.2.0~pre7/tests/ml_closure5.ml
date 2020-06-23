(* closure test *)
[@@@SCaml iml_optimization=false]
open SCaml

let f b = 
  if b then fun () -> b (* this should create a closure *)
  else fun () -> false

let [@entry] main x y = [], (assert ((f true) ()))
