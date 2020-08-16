module Angstrom = struct
  include Angstrom

  let both a b = lift2 (fun a b -> (a, b)) a b
end
