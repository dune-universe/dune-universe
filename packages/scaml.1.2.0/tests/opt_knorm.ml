open SCaml

let f b = if b then assert false else Int 2

let [@entry] main () () =
  [], ((let _ = f false in ()); ())

