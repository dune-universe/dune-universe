open SCaml

let [@entry] main () () =
  [], assert (Option.get (Some (Int 1)) = Int 1)
