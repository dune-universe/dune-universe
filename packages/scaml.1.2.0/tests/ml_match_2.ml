[@@@SCaml iml_optimization=false]
open SCaml
let [@entry] main param storage =
  [],
  assert (
    match (Left (Int 1) : (int, (int, unit) sum) sum) with
      | Left _ -> true
      | _ -> false
  )
