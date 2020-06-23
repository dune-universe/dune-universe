[@@@SCaml iml_optimization=false]
open SCaml
let main x y = 
  [],
  assert (
    Int.(+) (Int 1) (Int 2) = Int 3
    && Int.(-) (Int 1) (Int 2) = Int (-1)
    && Int.( * ) (Int 2) (Int 3) = Int 6
    && Int.(/) (Int 6) (Int 3) = Int 2
    && (match Int.ediv (Int 7) (Int 3) with Some (Int 2, Nat 1) -> true | _ -> false)
    && Int.(~-) (Int 2) = Int (-2)
    && Int.abs (Int (-2)) = Nat 2
    && (match Int.is_nat (Int 2) with Some (Nat 2) -> true | _ -> false)
    && Int.of_nat (Nat 3) = Int 3
    && Int.of_mutez (Tz 1.) = Int 1000000
  )
