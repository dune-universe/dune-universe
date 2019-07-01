let pp_hex_le fmt cs =
  let n = Cstruct.len cs in
  for i = n - 1 downto 0 do
    let byte = Cstruct.get_uint8 cs i in
    Format.fprintf fmt "%02x" byte
  done

let compare_be a b =
  let first_diff = ref None in
  let a_len = Cstruct.len a in
  let b_len = Cstruct.len b in
  if a_len <> b_len then invalid_arg "compare_be";
  for i = a_len - 1 downto 0 do
    let a_i = Cstruct.get_uint8 a i in
    let b_i = Cstruct.get_uint8 b i in
    match Pervasives.compare a_i b_i with
    | 0 ->
        ()
    | d ->
        first_diff := Some d
  done;
  match !first_diff with
  | None ->
      0
  | Some d ->
      d

let%expect_test "compare_be" =
  let test a b = print_int (compare_be a b) in
  test (Cstruct.of_string "aa") (Cstruct.of_string "ab");
  [%expect {| -1 |}];
  test (Cstruct.of_string "ab") (Cstruct.of_string "aa");
  [%expect {| 1 |}];
  test (Cstruct.of_string "aa") (Cstruct.of_string "aa");
  [%expect {| 0 |}];
  test (Cstruct.of_string "abx") (Cstruct.of_string "aaz");
  [%expect {| 1 |}];
  ()
