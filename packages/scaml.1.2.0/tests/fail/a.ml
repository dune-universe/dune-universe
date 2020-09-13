open SCaml

let main () () =
  let x = match Left (Int 1) with
    | Left x -> x
    | Right y -> y
  in
  let y = x + (Int 1) in
  [], ()
