module Int = struct
  module Spe = Reference.Specialize(Int)
  module Ringoid = Reference.SpecializeRingoid(Int)

  module O = struct
    include Spe.O
    include Ringoid.O
  end

  include (Spe: module type of Spe with module O := O)
  include (Ringoid: module type of Ringoid with type t := t and module O := O)

  let increment = OCSP.incr
  let decrement = OCSP.decr
end

module Float = struct
  module Spe = Reference.Specialize(Float)
  module Ringoid = Reference.SpecializeRingoid(Float)

  module O = struct
    include Spe.O
    include Ringoid.O
  end

  include (Spe: module type of Spe with module O := O)
  include (Ringoid: module type of Ringoid with type t := t and module O := O)
end

module String = struct
  module Spe = Reference.Specialize(String)

  module O = struct
    include Spe.O

    let (=^) r x =
      r := !r ^ x
  end

  include (Spe: module type of Spe with module O := O)
end
