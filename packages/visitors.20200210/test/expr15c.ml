open Expr15
open Expr15b

let () =
  Printf.printf "%d\n" (size (EAdd (EConst 22, EConst 11)))
