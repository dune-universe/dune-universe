module Cs = Cstruct

module Native = struct
  external chacha_core : int -> Cs.buffer -> Cs.buffer -> unit = "caml_chacha_core" [@@noalloc]
end

let chacha count i =
  let l = 64 in
  if Cs.len i <> l then
    invalid_arg "input must be 16 blocks of 32 bits"
  else
    let o = Cs.create l in
    Native.chacha_core count (Cs.to_bigarray i) (Cs.to_bigarray o);
    o

let chacha20 = chacha 10
let chacha12 = chacha 6
let chacha8 = chacha 4
