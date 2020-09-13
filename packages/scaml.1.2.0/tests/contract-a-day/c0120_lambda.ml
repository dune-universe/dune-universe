open SCaml

let main is _ =
  let incr x = x + Int 1 in
  [], List.map incr is
