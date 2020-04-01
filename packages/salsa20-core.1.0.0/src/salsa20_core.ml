open Cstruct

module Salsa_Core = struct
  external salsa_core : int -> buffer -> buffer -> unit = "caml_salsa_core" [@@noalloc]
end

let salsa20_core count i =
  let l = 64 in
  if len i <> l then invalid_arg "input must be 16 blocks of 32 bits"
  else
    let o = create l in
    Salsa_Core.salsa_core count (to_bigarray i) (to_bigarray o);
    o

let salsa20_8_core i =
  salsa20_core 4 i

let salsa20_12_core i =
  salsa20_core 6 i

let salsa20_20_core i =
  salsa20_core 10 i
