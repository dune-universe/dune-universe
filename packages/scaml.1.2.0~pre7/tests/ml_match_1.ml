[@@@SCaml iml_optimization=false]
open SCaml
let [@entry] main param storage =
  [],
  assert (
    ( match (Left (Int 1) : (int, unit) sum) with
      | Left x -> Int 1 = x
      | Right y -> false 
    )
  )
