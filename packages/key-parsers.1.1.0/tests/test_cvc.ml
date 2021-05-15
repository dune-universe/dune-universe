open OUnit2
open Test_helpers

let rsa_suite =
  let open Key_parsers in
  let open Cvc.Rsa in
  let expected_public =
    let n =
      Z.of_string
        "0x0102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f202122\
         232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f404142434445\
         464748494a4b4c4d4e4f505152535455565758595a5b5c5d5e5f606162636465666768\
         696a6b6c6d6e6f707172737475767778797a7b7c7d7e7f80"
    in
    let e = Z.of_string "0x010203" in
    Public.{n; e}
  in
  let cmp = Z.equal in
  let printer = Z.to_string in
  let test_pub ~decode (expected : Public.t) cvc ctxt =
    let real = decode cvc in
    let open Public in
    Test_util.assert_ok real @@ fun real ->
    assert_equal ~ctxt ~cmp ~printer ~msg:"n" expected.n real.n;
    assert_equal ~ctxt ~cmp ~printer ~msg:"e" expected.e real.e
  in
  let cvc = fixture "rsa_cvc_dummy.key" in
  [ "Public" >:: test_pub ~decode:Cvc.Rsa.Public.decode expected_public cvc
  ]

let ec_suite =
  let open Key_parsers in
  let open Cvc in
  let open Ec in
  let expected_public =
    (* parameters from secp256r1, public_point_y generated using openssl ecparam and pkey *)
    let modulus =
      Z.of_string
        "0xffffffff00000001000000000000000000000000fffffffffffffffffffffffc"
    in
    let coefficient_a =
      Test_util.cstruct_of_hex
        "ffffffff00000001000000000000000000000000fffffffffffffffffffffffc"
    in
    let coefficient_b =
      Test_util.cstruct_of_hex
        "5ac635d8aa3a93e7b3ebbd55769886bc651d06b0cc53b0f63bce3c3e27d2604b"
    in
    let base_point_g =
      Test_util.cstruct_of_hex
        "046b17d1f2e12c4247f8bce6e563a440f277037d812deb33a0f4a13945d898c2964f\
         e342e2fe1a7f9b8ee7eb4a7c0f9e162bce33576b315ececbb6406837bf51f5"
    in
    let base_point_r_order =
      Z.of_string
        "0xffffffff00000000ffffffffffffffffbce6faada7179e84f3b9cac2fc632551"
    in
    let public_point_y =
      Test_util.cstruct_of_hex
        "04E266926DF905285452A5BB4DFC70286D621BF8AF33FA8D2D2E4C5F86BF9DF3BA54CF\
         49409095250A14A93B16D9B2F8B7D6E1C247CB939FE9F10924B54E0BA075"
    in
    let cofactor_f = Z.of_string "1" in
    let open Public in
    { modulus
    ; coefficient_a
    ; coefficient_b
    ; base_point_g
    ; base_point_r_order
    ; public_point_y
    ; cofactor_f
    }
  in
  let test_pub ~decode (expected : Public.t) cvc ctxt =
    let real = decode cvc in
    Test_util.assert_ok real @@ fun real ->
    let open Public in
    assert_equal ~ctxt ~printer:(show) ~cmp:(equal) expected real
  in
  let cvc = fixture "ecdsa_cvc_dummy.key" in
  [ "Public" >:: test_pub ~decode:Cvc.Ec.Public.decode expected_public cvc
  ]

let suite =
  [ "Rsa" >::: rsa_suite
  ; "Ec" >::: ec_suite
  ]

