open SCaml
    
let main () () =
  [],
  assert (Int 3 = (+) (Int 1) (Int 1) |> (+) (Int 1))
