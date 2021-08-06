module Salsa_Core = struct
  external salsa_core : int -> Cstruct.buffer -> Cstruct.buffer -> unit = "caml_salsa_core" [@@noalloc]
end

let salsa20_core count i =
  let l = 64 in
  if Cstruct.length i <> l then invalid_arg "input must be 16 blocks of 32 bits"
  else
    let o = Cstruct.create l in
    Salsa_Core.salsa_core count (Cstruct.to_bigarray i) (Cstruct.to_bigarray o);
    o

let salsa20_8_core i =
  salsa20_core 4 i

let salsa20_12_core i =
  salsa20_core 6 i

let salsa20_20_core i =
  salsa20_core 10 i
