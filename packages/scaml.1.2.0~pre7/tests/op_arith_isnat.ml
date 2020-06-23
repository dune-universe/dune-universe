open SCaml
let [@entry] main x y = 
  [],
  assert (
      match isnat (Int (-1)) with
      | Some _ -> false
      | None -> match isnat (Int 1) with
                | None -> false
                | Some x -> x = Nat 1
    )
