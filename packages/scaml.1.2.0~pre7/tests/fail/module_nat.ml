[@@@SCaml iml_optimization=false]
open SCaml
let main x y = 
  [],
  assert (
    Nat.(+) (Nat 1) (Nat 2) = Nat 3
    && Nat.(-) (Nat 1) (Nat 2) = Int (-1)
    && Nat.( * ) (Nat 2) (Nat 3) = Nat 6
    && Nat.(/) (Nat 6) (Nat 3) = Nat 2
    && (match Nat.ediv (Nat 7) (Nat 3) with Some (Nat 2, Nat 1) -> true | _ -> false)
    && (match Nat.of_int (Int 3) with Some (Nat 3) -> true | _ -> false)
    && Nat.of_mutez (Tz 1.) = Nat 1000000
  )
