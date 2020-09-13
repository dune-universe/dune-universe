open SCaml

let [@entry] main () () =
  [], assert (Sum.get_right (Right (Int 1)) = Int 1)
    
