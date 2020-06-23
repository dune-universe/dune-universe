[@@@SCaml iml_optimization=false]
open SCaml
open Timestamp
let [@entry] main x y =
  let now = Global.get_now () in
  [],
  assert (
    add now (Int 10)  = sub (add now (Int 20)) (Int 10)
    && diff (add now (Int 10)) now = Int 10
    && Timestamp "2019-09-11T08:30:23Z" < Timestamp "2019-09-11T08:30:24Z"
  )
