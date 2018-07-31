open OUnit2
open Reed_solomon_erasure__.Matrix

let make_random_matrix (size : int) : t =
  let data = Array.init size (fun _ -> Bytes.init size (fun _ -> char_of_int (Random.int 256))) in
  make_with_data data

let test_matrix_col_count _ =
  let m1 = make_with_data [|Bytes.of_string "\001\000\000"|] in
  let m2 = make_with_data [|Bytes.of_string "\000\000\000";
                            Bytes.of_string "\000\000\000"|] in
  let m3 = make 1 4 in

  assert_equal 3 (col_count m1);
  assert_equal 3 (col_count m2);
  assert_equal 4 (col_count m3)

let test_matrix_row_count _ =
  let m1 = make_with_data [|Bytes.of_string "\001\000\000"|] in
  let m2 = make_with_data [|Bytes.of_string "\000\000\000";
                            Bytes.of_string "\000\000\000"|] in
  let m3 = make 1 4 in

  assert_equal 1 (row_count m1);
  assert_equal 2 (row_count m2);
  assert_equal 1 (row_count m3)

let test_matrix_swap_rows _ =
  begin
    let m1     = make_with_data [|Bytes.of_string "\001\002\003";
                                  Bytes.of_string "\004\005\006";
                                  Bytes.of_string "\007\008\009"|] in
    let expect = make_with_data [|Bytes.of_string "\007\008\009";
                                  Bytes.of_string "\004\005\006";
                                  Bytes.of_string "\001\002\003"|] in

    swap_rows m1 0 2;
    assert_equal expect m1;
  end;
  begin
    let m1     = make_with_data [|Bytes.of_string "\001\002\003";
                                  Bytes.of_string "\004\005\006";
                                  Bytes.of_string "\007\008\009"|] in
    let expect = copy m1 in
    swap_rows m1 0 0;
    assert_equal expect m1;
    swap_rows m1 1 1;
    assert_equal expect m1;
    swap_rows m1 2 2;
    assert_equal expect m1;
  end

let test_inconsistent_row_sizes _ =
  try
    make_with_data [|Bytes.of_string "\001\000\000";
                     Bytes.of_string "\000\000";
                     Bytes.of_string "\000\000\001"|] |> ignore;
    assert_failure "Missing exception"
  with
  | Failure _ -> ()

let test_incompatible_multiply _ =
  try
    let m1 = make_with_data [|Bytes.of_string "\000\001";
                              Bytes.of_string "\000\001";
                              Bytes.of_string "\000\001"|] in
    let m2 = make_with_data [|Bytes.of_string "\000\001\002"|] in
    multiply m1 m2 |> ignore;
    assert_failure "Missing exception"
  with
  | Failure _ -> ()

let test_incompatible_augment _ =
  try
    let m1 = make_with_data [|Bytes.of_string "\000\001"|] in
    let m2 = make_with_data [|Bytes.of_string "\000\001";
                              Bytes.of_string "\002\003"|] in
    augment m1 m2 |> ignore;
    assert_failure "Missing exception"
  with
  | Failure _ -> ()

let test_matrix_identity _ =
  let m1 = identity 3 in
  let m2 = make_with_data [|Bytes.of_string "\001\000\000";
                            Bytes.of_string "\000\001\000";
                            Bytes.of_string "\000\000\001"|] in
  assert_equal m1 m2

let test_matrix_multiply _ =
  let m1 = make_with_data [|Bytes.of_string "\001\002";
                            Bytes.of_string "\003\004"|] in
  let m2 = make_with_data [|Bytes.of_string "\005\006";
                            Bytes.of_string "\007\008"|] in
  let actual = multiply m1 m2 in
  let expect = make_with_data [|Bytes.of_string "\011\022";
                                Bytes.of_string "\019\042"|] in
  assert_equal actual expect

let test_matrix_inverse_pass_cases _ =
  begin
    let m =
      match invert (make_with_data [|Bytes.of_string "\056\023\098";
                                     Bytes.of_string "\003\100\200";
                                     Bytes.of_string "\045\201\123"|]) with
      | Ok(m)    -> m
      | Error(_) -> failwith "Error" in
    let expect = make_with_data [|Bytes.of_string "\175\133\033";
                                  Bytes.of_string "\130\013\245";
                                  Bytes.of_string "\112\035\126"|] in
    assert_equal m expect
  end;
  begin
    let m =
      match invert (make_with_data [|Bytes.of_string "\001\000\000\000\000";
                                     Bytes.of_string "\000\001\000\000\000";
                                     Bytes.of_string "\000\000\000\001\000";
                                     Bytes.of_string "\000\000\000\000\001";
                                     Bytes.of_string "\007\007\006\006\001"|]) with
      | Ok(m)    -> m
      | Error(_) -> assert_failure "Inversion failed" in
    let expect = make_with_data [|Bytes.of_string "\001\000\000\000\000";
                                  Bytes.of_string "\000\001\000\000\000";
                                  Bytes.of_string "\123\123\001\122\122";
                                  Bytes.of_string "\000\000\001\000\000";
                                  Bytes.of_string "\000\000\000\001\000"|] in
    assert_equal m expect
  end

let test_matrix_inverse_non_square _ =
  try
    invert (make_with_data [|Bytes.of_string "\056\023";
                             Bytes.of_string "\003\100";
                             Bytes.of_string "\045\201"|]) |> ignore;
    assert_failure "Missing exception"
  with
  | Failure _ -> ()

let test_matrix_inverse_singular _ =
  match invert (make_with_data [|Bytes.of_string "\004\002";
                                 Bytes.of_string "\012\006"|]) with
  | Ok _    -> assert_failure "Failed to detect fail case"
  | Error _ -> ()

let suite =
  "matrix_tests">:::
  ["test_matrix_col_count">::          test_matrix_col_count;
   "test_matrix_row_count">::          test_matrix_row_count;
   "test_matrix_swap_rows">::          test_matrix_swap_rows;
   "test_inconsistent_row_sizes">::    test_inconsistent_row_sizes;
   "test_incompatible_multiply">::     test_incompatible_multiply;
   "test_incompatible_augment">::      test_incompatible_augment;
   "test_matrix_identity">::           test_matrix_identity;
   "test_matrix_multiply">::           test_matrix_multiply;
   "test_matrix_inverse_pass_cases">:: test_matrix_inverse_pass_cases;
   "test_matrix_inverse_non_square">:: test_matrix_inverse_non_square;
   "test_matrix_inverse_singular">::   test_matrix_inverse_singular;
  ]
