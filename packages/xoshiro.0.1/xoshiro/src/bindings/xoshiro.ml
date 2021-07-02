module Xoshiro256plusplus = Xoshiro256plusplus_bindings

include Xoshiro256plusplus

module Splitmix64 = Splitmix64_pure
(* no bindings version of Splitmix. probably not a problem as we're not looking
   for performance *)
