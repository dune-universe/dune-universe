[@@@SCaml iml_optimization=false]
open SCaml
let [@entry] main param storage =
  [],
  assert (
    (* XXX tuple is formed, which is redundant *)
    ( match (Left (Int 1) : (int, int) sum) with
      | Left x | Right x -> x = Int 1
    )
  )
