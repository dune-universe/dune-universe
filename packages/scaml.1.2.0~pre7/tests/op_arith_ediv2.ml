[@@@SCaml iml_optimization=false]
open SCaml
let [@entry] main () () = 
  [],
  assert ((Int 10) / (Int 3) = Int 3
         && (Nat 10) /^ (Nat 3) = Nat 3
         && (Tz 10.) /$ (Tz 3.) = Nat 3
         && (Tz 10.) /$^ (Nat 2) = Tz 5.
        )
