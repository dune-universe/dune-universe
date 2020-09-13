open SCaml

let [@entry] main () () = 
  [], 
  let incr = (+) (Int 1) in
  assert (incr (Int 2) = Int 3)

