open SCaml

module Z = struct
  module X = struct
    let x = Int 1
  end
end

module Y = struct
  let [@entry] default () () =
    [], assert (Z.X.x = Int 1)
end
