include (Bignum0 : module type of Bignum0 with module Stable := Bignum0.Stable)

module Stable = struct
  include Bignum0.Stable

  module Bigint = Bigint.Stable
  [@@deprecated "[since 2017-12] Use [Bigint.Stable]. \
                 The [Bignum.Stable.Bigint] sub-module is no longer needed."]

  module Bignum = struct
    include Bignum0.Stable
    module Std = Std
  end
  [@@deprecated "[since 2017-12] Use [Bignum.Stable]. \
                 The [Bignum.Stable.Bignum] sub-module is no longer needed."]
end

module Bignum0 = Bignum0
[@@deprecated "[since 2017-12] Use [Bignum]. \
               The [Bignum.Bignum0] sub-module is no longer needed."]

module Std = Std
[@@deprecated "[since 2017-12] Use [Bignum]. \
               The [Bignum.Std] sub-module is no longer needed."]
