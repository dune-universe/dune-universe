open SCaml

type parameter = A  | B

type moo = { foo : nat ; bar : string }

type storage = moo

let [@entry] main p s =
  let result = match p with
      | A -> {s with foo = Nat 5}
      | B -> s               
  in
  [], result
