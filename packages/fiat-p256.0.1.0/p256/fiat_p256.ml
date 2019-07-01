let dh ~scalar ~point =
  Point.x_of_finite_point (Montgomery_ladder.scalar_mult scalar point)

let base_point = Point.of_hex_exn Parameters.g

let public scalar = Montgomery_ladder.scalar_mult scalar base_point

let%expect_test "dh" =
  let test d p =
    Format.printf "%a\n" Cstruct_util.pp_hex_le (dh ~scalar:d ~point:p)
  in
  let d_a =
    Scalar.of_hex_exn
      (`Hex
        "200102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f")
  in
  let d_b =
    Scalar.of_hex_exn
      (`Hex
        "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f")
  in
  let p_a = public d_a in
  let p_b = public d_b in
  test d_b p_a;
  [%expect
    {| 2e3e4065a62a7f425aaf8aae3d158f367c733300b5002e0b62f4bc6260789e1b |}];
  test d_a p_b;
  [%expect
    {| 2e3e4065a62a7f425aaf8aae3d158f367c733300b5002e0b62f4bc6260789e1b |}];
  test d_a p_a;
  [%expect
    {| 2ea4e810837da217a5bfd05f01d12459eeda830b6e0dec7f8afa425c5b55c507 |}];
  test d_b p_b;
  [%expect
    {| a7666bcc3818472194460f7df22d80a5886da0e1679eac930175ce1ff733c7ca |}]

type error = Error.point_error

let pp_error = Error.pp_point_error

let check_point = function
  | Ok p
    when not (Point.is_infinity p) ->
      Ok p
  | Ok _ ->
      Error `At_infinity
  | Error _ as e ->
      e

let point_of_cs c = check_point (Point.of_cstruct c)

let point_to_cs = Point.to_cstruct

type secret = Scalar.t

let secret_of_cs = Scalar.of_cstruct

let rec generate_private_key ~rng () =
  let candidate = rng 4 in
  match secret_of_cs candidate with
  | Ok secret ->
      secret
  | Error `Invalid_length ->
      failwith "Fiat_p256.gen_key: generator returned an invalid length"
  | Error _ ->
      generate_private_key ~rng ()

let gen_key ~rng =
  let private_key = generate_private_key ~rng () in
  let public_key = public private_key in
  let to_send = point_to_cs public_key in
  (private_key, to_send)

let key_exchange secret received =
  match point_of_cs received with
  | Error _ as err ->
      err
  | Ok other_party_public_key ->
      Ok (dh ~scalar:secret ~point:other_party_public_key)
