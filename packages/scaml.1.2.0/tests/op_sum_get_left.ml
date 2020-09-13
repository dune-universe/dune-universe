open SCaml

let [@entry] main () () =
  [], assert (Sum.get_left (Left (Int 1)) = Int 1)
    
