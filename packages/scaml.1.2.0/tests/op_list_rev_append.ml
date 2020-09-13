open SCaml

let [@entry] main () () =
  [],
  let l1 = List.rev_append [Int 1; Int 2] [Int 3; Int 4] in
  let l2 = [Int 2; Int 1; Int 3; Int 4] in
  Loop.left (function
      | [], [] -> Right ()
      | x::xs, y::ys when x = y -> Left (xs, ys)
      | _ -> assert false) (l1, l2)
    

