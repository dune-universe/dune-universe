module Stdlib = struct
  let map_fst f (a,b) = f a, b
  let map_snd f (a,b) = a, f b
end

include Stdlib

