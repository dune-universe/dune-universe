module RngZ = Nocrypto.Rng.Z
module NumZ = Nocrypto.Numeric.Z

let __gen_min bits = "1" ^ Core.String.init (bits - 1) ~f:(Core.const '0')

let __gen_max bits = Core.String.init bits ~f:(Core.const '1')

let __random_bits bits =
  let _min_random = Z.of_string_base 2 @@ __gen_min bits in
  let _max_random = Z.of_string_base 2 @@ __gen_max bits in
  NumZ.to_cstruct_be @@ RngZ.gen_r _min_random _max_random

let key () = __random_bits 256

let iv () = __random_bits 160

let _ = Nocrypto_entropy_unix.initialize ()
