[@@@SCaml iml_optimization=false]
open SCaml
let [@entry] main param storage =
  [],
  assert (
   ( match true with
     | true -> true
     | _ -> false
   )
  )
