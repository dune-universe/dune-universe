[@@@SCaml iml_optimization=false]
open SCaml
let [@entry] main () () = 
  [],
  assert (
    match ediv_int_int (Int 10) (Int 3) with
    | None -> false
    | Some i_n ->
        fst i_n = Int 3
        && snd i_n = Nat 1
  )
