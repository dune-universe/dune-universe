(*
   STORAGE= Int 2
   INPUT= Int 3
*)
open SCaml
let [@entry] main p s = ([], s + p)
