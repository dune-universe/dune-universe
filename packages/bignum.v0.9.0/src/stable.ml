module Bigint = Bigint.Stable
module Bignum = struct
  include Bignum0.Stable

  (* So people can [open Bignum.Std] after the module [Bignum.Stable.Bigum] has shadowed
     the library [Bignum]. *)
  module Std = Std
end
