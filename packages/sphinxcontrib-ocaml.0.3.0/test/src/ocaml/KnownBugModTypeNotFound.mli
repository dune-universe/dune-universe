module type A = sig
  module type B = sig
    module type C = sig
      type t
    end

    module type Alias = C
  end
end

include A
