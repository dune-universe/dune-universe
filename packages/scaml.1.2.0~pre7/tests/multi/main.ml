open SCaml

let [@entry] main () () =
  [],
  assert (Myoption.from_Some_int (Some (Int 1)) = Int 1
         && Myoption.Nat.from_Some_nat (Some (Nat 1)) = Nat 1)


