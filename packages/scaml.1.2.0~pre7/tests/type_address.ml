(* address test *)
[@@@SCaml iml_optimization=false]
open SCaml
let [@entry] main x y = ([], assert (Address "tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" = Address "tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN"))


