[@@@SCaml iml_optimization=false]
open SCaml
let [@entry] main param storage =
  [],
  assert (
    ( match Int 1 with
      | Int 2 -> false
      | x -> true
    )
  )
