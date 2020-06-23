[@@@SCaml iml_optimization=false]
open SCaml

let [@entry] main x y =
  [],
  assert (
    (Int 1, Nat 1, Tz 1.) = (Int 1, Nat 1, Tz 1.)
                            
    &&
    
    match (Int 1, Nat 1, Tz 1.) with
    | (_, Nat 1, _) -> true
    | _ -> false
  )
