module RngZ = Nocrypto.Rng.Z
module NumZ = Nocrypto.Numeric.Z

let __gen_min bits = "1" ^ Core.String.init (bits - 1) ~f:(Core.const '0')

let __gen_max bits = Core.String.init bits ~f:(Core.const '1')

let __max_bits bits = Z.of_string_base 2 @@ __gen_max bits

let __min_bits bits = Z.of_string_base 2 @@ __gen_min bits

let __random_bits bits =
  let _min_random = __min_bits bits in
  let _max_random = __max_bits bits in
  NumZ.to_cstruct_be @@ RngZ.gen_r _min_random _max_random


let max_bits bits = NumZ.to_cstruct_be @@ __max_bits bits

let min_bits bits = NumZ.to_cstruct_be @@ __min_bits bits

let key () = __random_bits 256

let iv () = __random_bits 128

let _ = Nocrypto_entropy_unix.initialize ()
