[@@@SCaml iml_optimization=false]
open SCaml
let b = true
let [@entry] main () () =
  [],
  assert (
    (
      if false then (fun () -> b)
      else (fun () -> true)
    )
    ()
     
  )
