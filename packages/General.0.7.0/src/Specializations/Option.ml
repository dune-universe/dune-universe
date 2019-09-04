module Int = struct
  include Option.Specialize(Int)
end

module Float = struct
  include Option.Specialize(Float)
end

module String = struct
  include Option.Specialize(String)
end
