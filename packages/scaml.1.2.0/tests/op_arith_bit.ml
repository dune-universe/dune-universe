[@@@SCaml iml_optimization=false]
open SCaml
let [@entry] main () () =
  [],
  assert (
    Nat 3 lor Nat 4 = Nat 7 
    && Nat 3 land Nat 4 = Nat 0
    && land_int_nat (Int 3) (Nat 4) = Nat 0
    && Nat 4 lxor Nat 7 = Nat 3
    && lnot_nat (Nat 10) = lnot (Int 10)
  )
