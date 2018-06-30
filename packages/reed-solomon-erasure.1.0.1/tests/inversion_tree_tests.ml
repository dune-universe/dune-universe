open OUnit2
open Reed_solomon_erasure__
open Reed_solomon_erasure__.Inversion_tree
open Matrix_tests

let test_new_inversion_tree test_ctxt =
  let tree = make 3 2 in

  let expect = Matrix.make_with_data [|Bytes.of_string "\001\000\000";
                                       Bytes.of_string "\000\001\000";
                                       Bytes.of_string "\000\000\001"|] in

  assert_equal (Some expect) (get_inverted_matrix tree [||])

let test_get_inverted_matrix test_ctxt =
  let tree = make 3 2 in

  let matrix = match get_inverted_matrix tree [||] with
    | Some m -> m
    | None   -> failwith ""
  in

  let expect = Matrix.make_with_data [|Bytes.of_string "\001\000\000";
                                       Bytes.of_string "\000\001\000";
                                       Bytes.of_string "\000\000\001"|] in

  assert_equal expect matrix;

  let matrix = get_inverted_matrix tree [|1|] in
  assert_equal None matrix;

  let matrix = get_inverted_matrix tree [|1; 2|] in
  assert_equal None matrix;

  let matrix = Matrix.make 3 3 in
  let matrix_copy = Matrix.copy matrix in

  (match insert_inverted_matrix tree [|1|] matrix with
   | Ok _    -> ()
   | Error _ -> assert_failure ""
  );

  let cached_matrix = get_inverted_matrix tree [|1|] in
  assert_equal (Some matrix_copy) cached_matrix

let test_insert_inverted_matrix test_ctxt =
  let tree = make 3 2 in

  let matrix      = Matrix.make 3 3 in
  let matrix_copy = Matrix.copy matrix in

  (match insert_inverted_matrix tree [|1|] matrix with
   | Ok _ -> ()
   | Error _ -> assert_failure ""
  );
  (match insert_inverted_matrix tree [||] matrix_copy with
   | Ok _  -> assert_failure ""
   | Error _ -> ()
  );

  let matrix = Matrix.make 3 2 in
  (match insert_inverted_matrix tree [|2|] matrix with
   | Ok _ -> assert_failure ""
   | Error _ -> ()
  );

  let matrix = Matrix.make 3 3 in
  (match insert_inverted_matrix tree [|0;1|] matrix with
   | Ok _ -> ()
   | Error _ -> assert_failure ""
  )

let test_double_insert_inverted_matrix test_ctxt =
  let tree = make 3 2 in

  let matrix1 = Matrix_tests.make_random_matrix 3 in
  let matrix2 = Matrix_tests.make_random_matrix 3 in

  let matrix_copy1 = Matrix.copy matrix1 in
  let matrix_copy2 = Matrix.copy matrix2 in

  (match insert_inverted_matrix tree [|1|] matrix_copy1 with
   | Ok _ -> ()
   | Error _ -> assert_failure ""
  );
  (match insert_inverted_matrix tree [|1|] matrix_copy2 with
   | Ok _ -> ()
   | Error _ -> assert_failure ""
  );

  let cached_matrix = get_inverted_matrix tree [|1|] in
  assert_equal (Some matrix_copy2) cached_matrix

let test_extended_inverted_matrix test_ctxt =
  let tree = make 10 3 in
  let matrix = Matrix.make 10 10 in
  let matrix_copy = Matrix.copy matrix in
  let matrix2 = Matrix.make_with_data [|Bytes.of_string "\000\001\002\003\004\005\006\007\008\009";
                                        Bytes.of_string "\000\001\002\003\004\005\006\007\008\009";
                                        Bytes.of_string "\000\001\002\003\004\005\006\007\008\009";
                                        Bytes.of_string "\000\001\002\003\004\005\006\007\008\009";
                                        Bytes.of_string "\000\001\002\003\004\005\006\007\008\009";
                                        Bytes.of_string "\001\001\002\003\004\005\006\007\008\009";
                                        Bytes.of_string "\001\001\002\003\004\005\006\007\008\009";
                                        Bytes.of_string "\001\001\002\003\004\005\006\007\008\009";
                                        Bytes.of_string "\001\001\002\003\004\005\006\007\008\009";
                                        Bytes.of_string "\001\001\002\003\004\005\006\007\008\009"|] in
  let matrix2_copy = Matrix.copy matrix2 in
  let matrix3 = Matrix.make_with_data [|Bytes.of_string "\009\001\002\003\004\005\006\007\008\000";
                                        Bytes.of_string "\009\001\002\003\004\005\006\007\008\000";
                                        Bytes.of_string "\009\001\002\003\004\005\006\007\008\000";
                                        Bytes.of_string "\009\001\002\003\004\005\006\007\008\000";
                                        Bytes.of_string "\009\001\002\003\004\005\006\007\008\000";
                                        Bytes.of_string "\001\001\002\003\004\005\006\007\008\009";
                                        Bytes.of_string "\001\001\002\003\004\005\006\007\008\009";
                                        Bytes.of_string "\001\001\002\003\004\005\006\007\008\009";
                                        Bytes.of_string "\001\001\002\003\004\005\006\007\008\009";
                                        Bytes.of_string "\001\001\002\003\004\005\006\007\008\009"|] in
  let matrix3_copy = Matrix.copy matrix3 in

  (match insert_inverted_matrix tree [|1; 2|] matrix with
   | Ok _ -> ()
   | Error _ -> assert_failure ""
  );

  let result = get_inverted_matrix tree [|1; 2|] in
  assert_equal (Some matrix_copy) result;

  (match insert_inverted_matrix tree [|1; 2; 5; 12|] matrix2 with
   | Ok _ -> ()
   | Error _ -> assert_failure ""
  );

  let result = get_inverted_matrix tree [|1; 2; 5; 12|] in
  assert_equal (Some matrix2_copy) result;

  (match insert_inverted_matrix tree [|0; 3; 4; 11|] matrix3 with
   | Ok _ -> ()
   | Error _ -> assert_failure ""
  );

  let result = get_inverted_matrix tree [|0; 3; 4; 11|] in
  assert_equal (Some matrix3_copy) result

let make_random_invalid_indices
    (data_shards   : int)
    (parity_shards : int)
  : int list =
  let invalid_count = ref 0 in
  let res = ref [] in
  for i = 0 to (data_shards + parity_shards) - 1 do
    if Random.bool () && !invalid_count < parity_shards then (
      res := i :: !res;
      invalid_count := !invalid_count + 1;
    )
  done;
  (List.rev !res)

let qc_tree_same_as_hashtbl =
  QCheck_runner.to_ounit2_test
    (QCheck.Test.make ~count:3000 ~name:"qc_tree_same_as_hashtbl"
       QCheck.(triple (int_range 1 255) (int_range 1 255) (triple (int_range 5 100) (list_of_size (QCheck.Gen.int_range 0 100) small_nat) (int_range 2 10)))
       (fun (data_shards, parity_shards, (matrix_count, iter_order, read_count)) ->
          QCheck.assume (data_shards > 0);
          QCheck.assume (parity_shards > 0);
          QCheck.assume (data_shards + parity_shards <= 256);
          let tree = make data_shards parity_shards in
          let map = Hashtbl.create matrix_count in

          let invalid_indices_set = ref [] in

          for _ = 0 to (matrix_count) - 1 do
            let invalid_indices = make_random_invalid_indices data_shards parity_shards in
            let matrix = make_random_matrix data_shards in
            match insert_inverted_matrix tree (Array.of_list invalid_indices) matrix with
            | Ok _ -> (Hashtbl.replace map invalid_indices matrix;
                       invalid_indices_set := invalid_indices :: !invalid_indices_set;
                      )
            | Error AlreadySet -> ()
            | Error NotSquare  -> assert_failure "Not square"
          done;

          let invalid_indices_set = Array.of_list !invalid_indices_set in

          let res = ref true in
          let counter = ref 0 in
          while !counter < read_count && !res do
            (* iterate according to the provided order *)
            if Array.length invalid_indices_set > 0 then (
              List.iter
                (fun i ->
                   let i = i mod (Array.length invalid_indices_set) in

                   let invalid_indices = invalid_indices_set.(i) in

                   let matrix_in_tree =
                     get_inverted_matrix tree (Array.of_list invalid_indices) in
                   let matrix_in_map = Hashtbl.find map invalid_indices in
                   if matrix_in_tree <> Some matrix_in_map then
                     res := false
                )
                iter_order
            );

            (* iterate through the insertion order *)
            Array.iter
              (fun invalid_indices ->
                 let matrix_in_tree =
                   get_inverted_matrix tree (Array.of_list invalid_indices) in
                 let matrix_in_map =
                   Hashtbl.find map invalid_indices in
                 if matrix_in_tree <> Some matrix_in_map then
                   res := false
              )
              invalid_indices_set;

            (* iterate through the map's order *)
            Hashtbl.iter
              (fun invalid_indices matrix_in_map ->
                 let matrix_in_tree =
                   get_inverted_matrix tree (Array.of_list invalid_indices) in
                 if matrix_in_tree <> Some matrix_in_map then
                   res := false
              )
              map;

            counter := !counter + 1
          done;

          !res
       ))

let suite =
  "inversion_tree_tests">:::
  ["test_new_inversion_tree">::            test_new_inversion_tree;
   "test_get_inverted_matrix">::           test_get_inverted_matrix;
   "test_insert_inverted_matrix">::        test_insert_inverted_matrix;
   "test_double_insert_inverted_matrix">:: test_double_insert_inverted_matrix;
   "test_extended_inverted_matrix">::      test_extended_inverted_matrix;
   qc_tree_same_as_hashtbl;
  ]
