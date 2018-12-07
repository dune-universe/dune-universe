open OUnit

open Rfc7748

let module_suite: (module DH) -> test list = fun m ->
  let module M = (val m) in
  let check_key_size _ =
    assert_equal ~printer:string_of_int
      M.((string_of_public_key base |> String.length))
      M.(key_size * 2)
  in
  let check_base_point _ =
    (* TODO: use a random value instead of a made-up one. *)
    let priv = M.private_key_of_string @@ String.make M.(key_size * 2) '4' in
    assert_equal ~printer:(M.string_of_public_key)
      M.(public_key_of_private_key priv)
      M.(scale priv base)
  in
  [ "key_size" >:: check_key_size
  ; "base_point" >:: check_base_point]

let x25519_dh _ =
  let base = "09" in
  let alice = "77076d0a7318a57d3c16c17251b26645df4c2f87ebc0992ab177fba51db92c2a" in
  let bob = "5dab087e624a8a4b79e17f8b83800ee66f3bb1292618b6fd1c2f8b27ff88e0eb" in
  let shared_secret_alice = x25519 ~priv:alice ~pub:(x25519 ~priv:bob ~pub:base) in
  let shared_secret_bob = x25519 ~priv:bob ~pub:(x25519 ~priv:alice ~pub:base) in
  assert_equal shared_secret_alice shared_secret_bob;
  assert_equal
    "4a5d9d5ba4ce2de1728e3bf480350f25e07e21c947d19e3376f09b3c1e161742"
    shared_secret_alice

let x448_dh _ =
  let base = "05" in
  let alice = "9a8f4925d1519f5775cf46b04b5800d4ee9ee8bae8bc5565d498c28dd9c9baf574a9419744897391006382a6f127ab1d9ac2d8c0a598726b" in
  let bob = "1c306a7ac2a0e2e0990b294470cba339e6453772b075811d8fad0d1d6927c120bb5ee8972b0d3e21374c9c921b09d1b0366f10b65173992d" in
  let shared_secret_alice = x448 ~priv:alice ~pub:(x448 ~priv:bob ~pub:base) in
  let shared_secret_bob = x448 ~priv:bob ~pub:(x448 ~priv:alice ~pub:base) in
  assert_equal shared_secret_alice shared_secret_bob;
  assert_equal
    "07fff4181ac6cc95ec1c16a94a0f74d12da232ce40a77552281d282bb60c0b56fd2464c335543936521c24403085d59a449a5037514a879d"
    shared_secret_alice

let _ =
  "Library_Suite" >::: [ "x25519" >::: module_suite (module X25519)
                       ; "x25519_dh" >:: x25519_dh
                       ; "x448" >::: module_suite (module X448)
                       ; "x448_dh" >:: x448_dh
                       ]
  |> run_test_tt_main
