open SCaml

module X = struct
  let x = Int 1
end

let [@entry] default () () =
  [], assert (X.x = Int 1)
