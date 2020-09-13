[@@@SCaml iml_optimization=false]
open SCaml
let [@entry] main x y =
  [],
  assert ("hello world" = String.concat "hello " "world"
         && "hello world" = "hello " ^ "world")
