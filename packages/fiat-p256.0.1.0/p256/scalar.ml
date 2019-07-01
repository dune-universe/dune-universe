type t = Scalar of Cstruct.t

let pp fmt (Scalar s) = Cstruct_util.pp_hex_le fmt s

let is_in_range cs =
  let zero = Cstruct.create 32 in
  let n = Hex.to_cstruct Parameters.n in
  Cstruct_util.compare_be cs zero > 0 && Cstruct_util.compare_be n cs > 0

let of_cstruct cs =
  if Cstruct.len cs <> 32 then Error `Invalid_length
  else if is_in_range cs then Ok (Scalar (Cstruct.rev cs))
  else Error `Invalid_range

let of_hex h =
  let cs = Hex.to_cstruct h in
  of_cstruct cs

let pp_err pp fmt = function
  | Error e ->
      Error.pp_scalar_error fmt e
  | Ok x ->
      pp fmt x

let%expect_test "of_hex" =
  let test h =
    let s = of_hex h in
    Format.printf "%a\n" (pp_err pp) s
  in
  test
    (`Hex "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f");
  [%expect
    {| 000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f |}];
  test
    (`Hex "0000000000000000000000000000000000000000000000000000000000000003");
  [%expect
    {| 0000000000000000000000000000000000000000000000000000000000000003 |}];
  test
    (`Hex
      "2000000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f");
  [%expect {| Cannot parse scalar: invalid length |}];
  test
    (`Hex "0000000000000000000000000000000000000000000000000000000000000000");
  [%expect {| Cannot parse scalar: invalid range |}];
  test
    (`Hex
      (* n-1 *)
      "FFFFFFFF00000000FFFFFFFFFFFFFFFFBCE6FAADA7179E84F3B9CAC2FC632550");
  [%expect
    {| ffffffff00000000ffffffffffffffffbce6faada7179e84f3b9cac2fc632550 |}];
  test Parameters.n;
  [%expect {| Cannot parse scalar: invalid range |}];
  test
    (`Hex
      (* n+1 *)
      "FFFFFFFF00000000FFFFFFFFFFFFFFFFBCE6FAADA7179E84F3B9CAC2FC632552");
  [%expect {| Cannot parse scalar: invalid range |}]

let of_hex_exn h =
  match of_hex h with
  | Ok p ->
      p
  | Error e ->
      failwith (Format.asprintf "of_hex_exn: %a" Error.pp_scalar_error e)

let bit_at (Scalar s) i =
  let byte_num = i / 8 in
  let bit_num = i mod 8 in
  let byte = Cstruct.get_uint8 s byte_num in
  byte land (1 lsl bit_num) <> 0
