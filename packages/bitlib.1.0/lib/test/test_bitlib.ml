open Alcotest
open Bitlib

let to_bits_test () =
  (check (list int)) "to bits" [1;0;1] (to_bits 5)

let pad_test () =
  (check (list int)) "pad" [0;0;0;0;0;1;0;1] (pad 8 (to_bits 5))

let to_bytes_test () =
  let bits = pad 32 (to_bits 5) in
  let res = to_bytes bits 4 in
  (check (list int)) "to bytes" [0;0;0;5] res;
  let bits = pad 32 (to_bits 256) in
  let res = to_bytes bits 4 in
  (check (list int)) "to bytes" [0;0;1;0] res

let little_endian_of_int_test () =
  (check (list int)) "little endian" [0;0;1;1] (little_endian_of_int 257 4);
  (check (list int)) "little endian" [0;5] (little_endian_of_int 5 2)

let big_endian_of_int_test () =
  (check (list int)) "big endian" [1;2;0;0] (big_endian_of_int 513 4);
  (check (list int)) "big endian" [5;0] (big_endian_of_int 5 2)

let tests = [
  "to_bits", `Quick, to_bits_test;
  "pad", `Quick, pad_test;
  "to_bytes", `Quick, to_bytes_test;
  "little_endian_of_int", `Quick, little_endian_of_int_test;
  "big_endian_of_int", `Quick, big_endian_of_int_test;
]
