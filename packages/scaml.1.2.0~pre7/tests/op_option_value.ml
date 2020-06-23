open SCaml

let [@entry] main () () =
  [],
  assert (
    Option.value (Some (Int 1)) (Int 2) = Int 1
    &&
    Option.value None (Int 2) = Int 2
  )
