[@@@SCaml iml_optimization=false]
open SCaml
let [@entry] main () () = 
  [],
  assert ( 
    match 
      List.rev [ Int 1; Int 2; Int 3 ]
    with
    | [] -> false
    | x::xs ->
        x = Int 3 
        && match xs with
           | [] -> false
           | x::xs ->
               x = Int 2
               && match xs with
               | [] -> false
               | x::xs ->
                   x = Int 1
                   && match xs with
                      | [] -> true
                      | _::_ -> false
  )
                        

