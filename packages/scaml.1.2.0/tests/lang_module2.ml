open SCaml

module X = struct
  let x = Int 1
end

module Y = struct
  let [@entry] default () () =
    [], assert (X.x = Int 1)
end
