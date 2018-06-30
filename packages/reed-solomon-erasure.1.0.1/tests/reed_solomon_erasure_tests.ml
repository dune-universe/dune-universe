open OUnit2
open Reed_solomon_erasure
open Reed_solomon_erasure__.Ops
open Reed_solomon_erasure.RS_Shard_utils
module Bigstring = Core_kernel.Bigstring

let array_split_at (arr : 'a array) (split_at : int) : 'a array * 'a array =
  (Array.sub arr 0        split_at,
   Array.sub arr split_at ((Array.length arr) - split_at))

let make_random_shards_str per_shard size =
  Array.init
    size
    (fun _ ->
       String.init
         per_shard
         (fun _ -> char_of_int (Random.int 256)))

let make_random_shards_bytes per_shard size =
  Array.init
    size
    (fun _ ->
       Bytes.init
         per_shard
         (fun _ -> char_of_int (Random.int 256)))

let make_random_shards_bigstr per_shard size =
    (Array.init
       size
       (fun _ ->
          Bigstring.of_bytes
            (Bytes.init
               per_shard
               (fun _ -> char_of_int (Random.int 256)))))

let fill_random slice =
  for i = 0 to (Bytes.length slice) - 1 do
    slice.%(i) <- char_of_int (Random.int 256)
  done

let fill_random_bigstr slice =
  for i = 0 to (Bigstring.length slice) - 1 do
    slice.{i} <- char_of_int (Random.int 256)
  done

let print_shards shards =
  Array.iter
    (fun s ->
       Printf.printf "[";
       Bytes.iter
         (fun b -> Printf.printf "%d, " (int_of_char b))
         s;
       Printf.printf "]\n";
    )
    shards

let test_no_data_shards test_ctxt =
  assert_equal (Error RS_Error.TooFewDataShards) (ReedSolomon.make_no_exn 0 1)

let test_negative_data_shards test_ctxt =
  assert_equal (Error RS_Error.TooFewDataShards) (ReedSolomon.make_no_exn (-1) 1)

let test_no_parity_shards test_ctxt =
  assert_equal (Error RS_Error.TooFewParityShards) (ReedSolomon.make_no_exn 1 0)

let test_negative_parity_shards test_ctxt =
  assert_equal (Error RS_Error.TooFewParityShards) (ReedSolomon.make_no_exn 1 (-1))

let test_too_many_shards test_ctxt =
  assert_equal (Error RS_Error.TooManyShards) (ReedSolomon.make_no_exn 129 128)

let test_shard_count test_ctxt =
  for _ = 0 to (10) - 1 do
    let data_shard_count   = 1 + Random.int 128 in
    let parity_shard_count = 1 + Random.int 128 in

    let total_shard_count = data_shard_count + parity_shard_count in

    let r = ReedSolomon.make data_shard_count parity_shard_count in

    assert_equal data_shard_count   (ReedSolomon.data_shard_count   r);
    assert_equal parity_shard_count (ReedSolomon.parity_shard_count r);
    assert_equal total_shard_count  (ReedSolomon.total_shard_count  r)
  done

let test_shards_to_option_shards_to_shards test_ctxt =
  for _ = 0 to (100) - 1 do
    let shards = make_random_shards_bytes 1_000 10 in
    let expect = copy_shards_bytes shards in
    let inter =
      RS_Shard_utils.shards_to_option_shards_bytes shards in
    let result =
      RS_Shard_utils.option_shards_to_shards_bytes inter in

    assert_equal expect result
  done

let test_option_shards_to_shards_missing_shards_case1 test_ctxt =
  let shards = make_random_shards_bytes 1_000 10 in
  let option_shards = RS_Shard_utils.shards_to_option_shards_bytes shards in

  option_shards.(0) <- None;

  try
    RS_Shard_utils.option_shards_to_shards_bytes option_shards |> ignore;
    assert_failure "Missing exception"
  with
  | Failure _ -> ()

let test_option_shards_to_shards_missing_shards_case2 test_ctxt =
  let shards = make_random_shards_bytes 1_000 10 in
  let option_shards = RS_Shard_utils.shards_to_option_shards_bytes shards in

  option_shards.(0) <- None;
  option_shards.(9) <- None;

  RS_Shard_utils.option_shards_to_shards_bytes (Array.sub option_shards 1 8) |> ignore

let test_option_shards_to_missing_shards test_ctxt =
  let shards = make_random_shards_bytes 1_000 10 in
  let option_shards = RS_Shard_utils.shards_to_option_shards_bytes shards in

  option_shards.(2) <- None;

  try
    RS_Shard_utils.option_shards_to_shards_bytes option_shards |> ignore;
    assert_failure "Missing exception"
  with
  | Failure _ -> ()

let test_reed_solomon_copy test_ctxt =
  ()

let test_encoding test_ctxt =
  let per_shard = 50_000 in

  let r = ReedSolomon.make 10 3 in

  begin (* bytes *)
    let shards = make_random_shards_bytes per_shard 13 in

    ReedSolomon.encode_bytes r shards;
    assert_bool "verification" (ReedSolomon.verify_bytes r shards);

    assert_equal (Error RS_Error.TooFewShards) (ReedSolomon.encode_bytes_no_exn r (Array.sub shards 0 1));

    let bad_shards = make_random_shards_bytes per_shard 13 in
    bad_shards.(0) <- Bytes.make 1 '\000';
    assert_equal (Error RS_Error.IncorrectShardSize) (ReedSolomon.encode_bytes_no_exn r bad_shards)
  end;
  begin (* str *)
    let shards = make_random_shards_str per_shard 13 in

    ReedSolomon.encode_str r shards;
    assert_bool "verification" (ReedSolomon.verify_str r shards);

    assert_equal (Error RS_Error.TooFewShards) (ReedSolomon.encode_str_no_exn r (Array.sub shards 0 1));

    let bad_shards = make_random_shards_str per_shard 13 in
    bad_shards.(0) <- String.make 1 '\000';
    assert_equal (Error RS_Error.IncorrectShardSize) (ReedSolomon.encode_str_no_exn r bad_shards)
  end;
  begin (* bigstr *)
    let shards = make_random_shards_bigstr per_shard 13 in

    ReedSolomon.encode_bigstr r shards;
    assert_bool "verification" (ReedSolomon.verify_bigstr r shards);

    assert_equal (Error RS_Error.TooFewShards) (ReedSolomon.encode_bigstr_no_exn r (Array.sub shards 0 1));

    let bad_shards = make_random_shards_bytes per_shard 13 in
    bad_shards.(0) <- Bytes.make 1 '\000';
    assert_equal (Error RS_Error.IncorrectShardSize) (ReedSolomon.encode_bytes_no_exn r bad_shards)
  end

let test_reconstruct_opt test_ctxt =
  let per_shard = 100_000 in

  let r = ReedSolomon.make 8 5 in

  begin (* bytes *)
    let shards = make_random_shards_bytes per_shard 13 in

    ReedSolomon.encode_bytes r shards;

    let master_copy = copy_shards_bytes shards in

    let shards = RS_Shard_utils.shards_to_option_shards_bytes shards in

    begin
      let shards = RS_Shard_utils.option_shards_to_shards_bytes shards in
      assert_equal shards master_copy;
    end;

    (* Try to decode with all shards present *)
    ReedSolomon.reconstruct_opt_bytes r shards;
    begin
      let shards = RS_Shard_utils.option_shards_to_shards_bytes shards in
      assert_bool "verification" (ReedSolomon.verify_bytes r shards);
      assert_equal shards master_copy;
    end;

    (* Try to decode with 10 shards *)
    shards.(0) <- None;
    shards.(2) <- None;
    (* shards.(4) <- None; *)
    ReedSolomon.reconstruct_opt_bytes r shards;
    begin
      let shards = RS_Shard_utils.option_shards_to_shards_bytes shards in
      assert_bool "verification" (ReedSolomon.verify_bytes r shards);
      assert_equal shards master_copy;
    end;

    (* Try to decode the same shards again to try to
     * trigger the usage of cached decode matrix *)
    shards.(0) <- None;
    shards.(2) <- None;
    (* shards.(4) <- None; *)
    ReedSolomon.reconstruct_opt_bytes r shards;
    begin
      let shards = RS_Shard_utils.option_shards_to_shards_bytes shards in
      assert_bool "verification" (ReedSolomon.verify_bytes r shards);
      assert_equal shards master_copy;
    end;

    (* Try to deocde with 6 data and 4 parity shards *)
    shards.(0) <- None;
    shards.(2) <- None;
    shards.(12) <- None;
    ReedSolomon.reconstruct_opt_bytes r shards;
    begin
      let shards = RS_Shard_utils.option_shards_to_shards_bytes shards in
      assert_bool "verification" (ReedSolomon.verify_bytes r shards);
      assert_equal shards master_copy;
    end;

    (* Try to reconstruct data only *)
    shards.(0) <- None;
    shards.(1) <- None;
    shards.(12) <- None;
    ReedSolomon.reconstruct_data_opt_bytes r shards;
    begin
      let data_shards = RS_Shard_utils.option_shards_to_shards_bytes (Array.sub shards 0 8) in
      assert_equal master_copy.(0) data_shards.(0);
      assert_equal master_copy.(1) data_shards.(1);
      assert_equal None            shards.(12);
    end;

    (* Try to decode with 7 data and 1 parity shards *)
    shards.(0) <- None;
    shards.(1) <- None;
    shards.(9) <- None;
    shards.(10) <- None;
    shards.(11) <- None;
    shards.(12) <- None;
    assert_equal (Error RS_Error.TooFewShardsPresent) (ReedSolomon.reconstruct_opt_bytes_no_exn r shards)
  end;
  begin (* str *)
    let shards = make_random_shards_str per_shard 13 in

    ReedSolomon.encode_str r shards;

    let master_copy = copy_shards_str shards in

    let shards = RS_Shard_utils.shards_to_option_shards_str shards in

    begin
      let shards = RS_Shard_utils.option_shards_to_shards_str shards in
      assert_equal shards master_copy;
    end;

    (* Try to decode with all shards present *)
    ReedSolomon.reconstruct_opt_str r shards;
    begin
      let shards = RS_Shard_utils.option_shards_to_shards_str shards in
      assert_bool "verification" (ReedSolomon.verify_str r shards);
      assert_equal shards master_copy;
    end;

    (* Try to decode with 10 shards *)
    shards.(0) <- None;
    shards.(2) <- None;
    (* shards.(4) <- None; *)
    ReedSolomon.reconstruct_opt_str r shards;
    begin
      let shards = RS_Shard_utils.option_shards_to_shards_str shards in
      assert_bool "verification" (ReedSolomon.verify_str r shards);
      assert_equal shards master_copy;
    end;

    (* Try to decode the same shards again to try to
     * trigger the usage of cached decode matrix *)
    shards.(0) <- None;
    shards.(2) <- None;
    (* shards.(4) <- None; *)
    ReedSolomon.reconstruct_opt_str r shards;
    begin
      let shards = RS_Shard_utils.option_shards_to_shards_str shards in
      assert_bool "verification" (ReedSolomon.verify_str r shards);
      assert_equal shards master_copy;
    end;

    (* Try to deocde with 6 data and 4 parity shards *)
    shards.(0) <- None;
    shards.(2) <- None;
    shards.(12) <- None;
    ReedSolomon.reconstruct_opt_str r shards;
    begin
      let shards = RS_Shard_utils.option_shards_to_shards_str shards in
      assert_bool "verification" (ReedSolomon.verify_str r shards);
      assert_equal shards master_copy;
    end;

    (* Try to reconstruct data only *)
    shards.(0) <- None;
    shards.(1) <- None;
    shards.(12) <- None;
    ReedSolomon.reconstruct_data_opt_str r shards;
    begin
      let data_shards = RS_Shard_utils.option_shards_to_shards_str (Array.sub shards 0 8) in
      assert_equal master_copy.(0) data_shards.(0);
      assert_equal master_copy.(1) data_shards.(1);
      assert_equal None            shards.(12);
    end;

    (* Try to decode with 7 data and 1 parity shards *)
    shards.(0) <- None;
    shards.(1) <- None;
    shards.(9) <- None;
    shards.(10) <- None;
    shards.(11) <- None;
    shards.(12) <- None;
    assert_equal (Error RS_Error.TooFewShardsPresent) (ReedSolomon.reconstruct_opt_str_no_exn r shards)
  end;
  begin (* bigstr *)
    let shards = make_random_shards_bigstr per_shard 13 in

    ReedSolomon.encode_bigstr r shards;

    let master_copy = copy_shards_bigstr shards in

    let shards = RS_Shard_utils.shards_to_option_shards_bigstr shards in

    begin
      let shards = RS_Shard_utils.option_shards_to_shards_bigstr shards in
      assert_equal shards master_copy;
    end;

    (* Try to decode with all shards present *)
    ReedSolomon.reconstruct_opt_bigstr r shards;
    begin
      let shards = RS_Shard_utils.option_shards_to_shards_bigstr shards in
      assert_bool "verification" (ReedSolomon.verify_bigstr r shards);
      assert_equal shards master_copy;
    end;

    (* Try to decode with 10 shards *)
    shards.(0) <- None;
    shards.(2) <- None;
    (* shards.(4) <- None; *)
    ReedSolomon.reconstruct_opt_bigstr r shards;
    begin
      let shards = RS_Shard_utils.option_shards_to_shards_bigstr shards in
      assert_bool "verification" (ReedSolomon.verify_bigstr r shards);
      assert_equal shards master_copy;
    end;

    (* Try to decode the same shards again to try to
     * trigger the usage of cached decode matrix *)
    shards.(0) <- None;
    shards.(2) <- None;
    (* shards.(4) <- None; *)
    ReedSolomon.reconstruct_opt_bigstr r shards;
    begin
      let shards = RS_Shard_utils.option_shards_to_shards_bigstr shards in
      assert_bool "verification" (ReedSolomon.verify_bigstr r shards);
      assert_equal shards master_copy;
    end;

    (* Try to deocde with 6 data and 4 parity shards *)
    shards.(0) <- None;
    shards.(2) <- None;
    shards.(12) <- None;
    ReedSolomon.reconstruct_opt_bigstr r shards;
    begin
      let shards = RS_Shard_utils.option_shards_to_shards_bigstr shards in
      assert_bool "verification" (ReedSolomon.verify_bigstr r shards);
      assert_equal shards master_copy;
    end;

    (* Try to reconstruct data only *)
    shards.(0) <- None;
    shards.(1) <- None;
    shards.(12) <- None;
    ReedSolomon.reconstruct_data_opt_bigstr r shards;
    begin
      let data_shards = RS_Shard_utils.option_shards_to_shards_bigstr (Array.sub shards 0 8) in
      assert_equal master_copy.(0) data_shards.(0);
      assert_equal master_copy.(1) data_shards.(1);
      assert_equal None            shards.(12);
    end;

    (* Try to decode with 7 data and 1 parity shards *)
    shards.(0) <- None;
    shards.(1) <- None;
    shards.(9) <- None;
    shards.(10) <- None;
    shards.(11) <- None;
    shards.(12) <- None;
    assert_equal (Error RS_Error.TooFewShardsPresent) (ReedSolomon.reconstruct_opt_bigstr_no_exn r shards)
  end

let test_reconstruct test_ctxt =
  let r = ReedSolomon.make 2 2 in

  begin (* bytes *)
    let shards = [|Bytes.of_string "\000\001\002";
                   Bytes.of_string "\003\004\005";
                   Bytes.of_string "\200\201\203";
                   Bytes.of_string "\100\101\102"|] in

    begin
      ReedSolomon.encode_bytes r shards;

      assert_bool "verification" (ReedSolomon.verify_bytes r shards)
    end;

    begin
      shards.(0).%(0) <- 101 |> char_of_int;
      shards.(0).%(1) <- 102 |> char_of_int;
      shards.(0).%(2) <- 102 |> char_of_int;

      let shards_present = [|false; true; true; true|] in

      ReedSolomon.reconstruct_bytes r shards shards_present;

      assert_bool "verification" (ReedSolomon.verify_bytes r shards)
    end;

    let expect = [|Bytes.of_string "\000\001\002";
                   Bytes.of_string "\003\004\005";
                   Bytes.of_string "\006\011\012";
                   Bytes.of_string "\005\014\011"|] in

    assert_equal expect shards;

    begin
      shards.(0).%(0) <- 201 |> char_of_int;
      shards.(0).%(1) <- 202 |> char_of_int;
      shards.(0).%(2) <- 203 |> char_of_int;

      shards.(2).%(0) <- 101 |> char_of_int;
      shards.(2).%(1) <- 102 |> char_of_int;
      shards.(2).%(2) <- 103 |> char_of_int;

      let shards_present = [|false; true; false; true|] in

      ReedSolomon.reconstruct_data_bytes r shards shards_present;

      assert_bool "verification" (not (ReedSolomon.verify_bytes r shards))
    end;

    let expect = [|Bytes.of_string "\000\001\002";
                   Bytes.of_string "\003\004\005";
                   Bytes.of_string "\101\102\103";
                   Bytes.of_string "\005\014\011"|] in

    assert_equal expect shards;

    begin
      shards.(2).%(0) <- 101 |> char_of_int;
      shards.(2).%(1) <- 102 |> char_of_int;
      shards.(2).%(2) <- 103 |> char_of_int;

      shards.(3).%(0) <- 201 |> char_of_int;
      shards.(3).%(1) <- 202 |> char_of_int;
      shards.(3).%(2) <- 203 |> char_of_int;

      let shards_present = [|true; true; false; false|] in

      ReedSolomon.reconstruct_data_bytes r shards shards_present;

      assert_bool "verification" (not (ReedSolomon.verify_bytes r shards))
    end;

    let expect = [|Bytes.of_string "\000\001\002";
                   Bytes.of_string "\003\004\005";
                   Bytes.of_string "\101\102\103";
                   Bytes.of_string "\201\202\203"|] in

    assert_equal expect shards
  end;
  begin (* str *)
    let shards = [|"\000\001\002";
                   "\003\004\005";
                   "\200\201\203";
                   "\100\101\102"|] in

    begin
      ReedSolomon.encode_str r shards;

      assert_bool "verification" (ReedSolomon.verify_str r shards)
    end;

    begin
      (Bytes.unsafe_of_string shards.(0)).%(0) <- 101 |> char_of_int;
      (Bytes.unsafe_of_string shards.(0)).%(1) <- 102 |> char_of_int;
      (Bytes.unsafe_of_string shards.(0)).%(2) <- 102 |> char_of_int;

      let shards_present = [|false; true; true; true|] in

      ReedSolomon.reconstruct_str r shards shards_present;

      assert_bool "verification" (ReedSolomon.verify_str r shards)
    end;

    let expect = [|"\000\001\002";
                   "\003\004\005";
                   "\006\011\012";
                   "\005\014\011"|] in

    assert_equal expect shards;

    begin
      (Bytes.unsafe_of_string shards.(0)).%(0) <- 201 |> char_of_int;
      (Bytes.unsafe_of_string shards.(0)).%(1) <- 202 |> char_of_int;
      (Bytes.unsafe_of_string shards.(0)).%(2) <- 203 |> char_of_int;

      (Bytes.unsafe_of_string shards.(2)).%(0) <- 101 |> char_of_int;
      (Bytes.unsafe_of_string shards.(2)).%(1) <- 102 |> char_of_int;
      (Bytes.unsafe_of_string shards.(2)).%(2) <- 103 |> char_of_int;

      let shards_present = [|false; true; false; true|] in

      ReedSolomon.reconstruct_data_str r shards shards_present;

      assert_bool "verification" (not (ReedSolomon.verify_str r shards))
    end;

    let expect = [|"\000\001\002";
                   "\003\004\005";
                   "\101\102\103";
                   "\005\014\011"|] in

    assert_equal expect shards;

    begin
      (Bytes.unsafe_of_string shards.(2)).%(0) <- 101 |> char_of_int;
      (Bytes.unsafe_of_string shards.(2)).%(1) <- 102 |> char_of_int;
      (Bytes.unsafe_of_string shards.(2)).%(2) <- 103 |> char_of_int;

      (Bytes.unsafe_of_string shards.(3)).%(0) <- 201 |> char_of_int;
      (Bytes.unsafe_of_string shards.(3)).%(1) <- 202 |> char_of_int;
      (Bytes.unsafe_of_string shards.(3)).%(2) <- 203 |> char_of_int;

      let shards_present = [|true; true; false; false|] in

      ReedSolomon.reconstruct_data_str r shards shards_present;

      assert_bool "verification" (not (ReedSolomon.verify_str r shards))
    end;

    let expect = [|"\000\001\002";
                   "\003\004\005";
                   "\101\102\103";
                   "\201\202\203"|] in

    assert_equal expect shards
  end;
  begin (* bigstr *)
    let shards = [|Bigstring.of_string "\000\001\002";
                   Bigstring.of_string "\003\004\005";
                   Bigstring.of_string "\200\201\203";
                   Bigstring.of_string "\100\101\102"|] in

    begin
      ReedSolomon.encode_bigstr r shards;

      assert_bool "verification" (ReedSolomon.verify_bigstr r shards)
    end;

    begin
      shards.(0).{0} <- 101 |> char_of_int;
      shards.(0).{1} <- 102 |> char_of_int;
      shards.(0).{2} <- 102 |> char_of_int;

      let shards_present = [|false; true; true; true|] in

      ReedSolomon.reconstruct_bigstr r shards shards_present;

      assert_bool "verification" (ReedSolomon.verify_bigstr r shards)
    end;

    let expect = [|Bigstring.of_string "\000\001\002";
                   Bigstring.of_string "\003\004\005";
                   Bigstring.of_string "\006\011\012";
                   Bigstring.of_string "\005\014\011"|] in

    assert_equal expect shards;

    begin
      shards.(0).{0} <- 201 |> char_of_int;
      shards.(0).{1} <- 202 |> char_of_int;
      shards.(0).{2} <- 203 |> char_of_int;

      shards.(2).{0} <- 101 |> char_of_int;
      shards.(2).{1} <- 102 |> char_of_int;
      shards.(2).{2} <- 103 |> char_of_int;

      let shards_present = [|false; true; false; true|] in

      ReedSolomon.reconstruct_data_bigstr r shards shards_present;

      assert_bool "verification" (not (ReedSolomon.verify_bigstr r shards))
    end;

    let expect = [|Bigstring.of_string "\000\001\002";
                   Bigstring.of_string "\003\004\005";
                   Bigstring.of_string "\101\102\103";
                   Bigstring.of_string "\005\014\011"|] in

    assert_equal expect shards;

    begin
      shards.(2).{0} <- 101 |> char_of_int;
      shards.(2).{1} <- 102 |> char_of_int;
      shards.(2).{2} <- 103 |> char_of_int;

      shards.(3).{0} <- 201 |> char_of_int;
      shards.(3).{1} <- 202 |> char_of_int;
      shards.(3).{2} <- 203 |> char_of_int;

      let shards_present = [|true; true; false; false|] in

      ReedSolomon.reconstruct_data_bigstr r shards shards_present;

      assert_bool "verification" (not (ReedSolomon.verify_bigstr r shards))
    end;

    let expect = [|Bigstring.of_string "\000\001\002";
                   Bigstring.of_string "\003\004\005";
                   Bigstring.of_string "\101\102\103";
                   Bigstring.of_string "\201\202\203"|] in

    assert_equal expect shards
  end

let qc_encode_verify_reconstruct_verify =
  QCheck_runner.to_ounit2_test
    (QCheck.Test.make ~count:500 ~name:"qc_encode_verify_reconstruct_verify"
       QCheck.(triple (int_range 1 255) (int_range 1 255) (triple pos_int (int_range 1 1000) unit))
       (fun (data, parity, (corrupt, size, ())) ->
          QCheck.assume (0 < data);
          QCheck.assume (0 < parity);
          QCheck.assume (data + parity <= 256);
          QCheck.assume (0 < size);

          let corrupt = corrupt mod (parity + 1) in

          let corrupt_pos_s = Array.make corrupt 0 in
          for i = 0 to (corrupt) - 1 do
            let pos = ref (Random.int (data + parity)) in

            while Array.mem !pos corrupt_pos_s do
              pos := Random.int (data + parity);
            done;

            corrupt_pos_s.(i) <- !pos;
          done;

          let r = ReedSolomon.make data parity in

          begin (* bytes *)
            let shards = make_random_shards_bytes size (data + parity) in
            ReedSolomon.encode_bytes r shards;
            let expect = copy_shards_bytes shards in

            (* corrupt shards *)
            Array.iter
              (fun p ->
                 fill_random shards.(p)
              )
              corrupt_pos_s;
            let shard_present = Array.make (data + parity) true in
            Array.iter
              (fun p ->
                 shard_present.(p) <- false;
              )
              corrupt_pos_s;

            (* reconstruct *)
            ReedSolomon.reconstruct_bytes r shards shard_present;

            (ReedSolomon.verify_bytes r expect)
            &&
            (expect = shards)
            &&
            (ReedSolomon.verify_bytes r shards)
          end
          &&
          begin (* str *)
            let shards = make_random_shards_str size (data + parity) in
            ReedSolomon.encode_str r shards;
            let expect = copy_shards_str shards in

            (* corrupt shards *)
            Array.iter
              (fun p ->
                 fill_random (Bytes.unsafe_of_string shards.(p))
              )
              corrupt_pos_s;
            let shard_present = Array.make (data + parity) true in
            Array.iter
              (fun p ->
                 shard_present.(p) <- false;
              )
              corrupt_pos_s;

            (* reconstruct *)
            ReedSolomon.reconstruct_str r shards shard_present;

            (ReedSolomon.verify_str r expect)
            &&
            (expect = shards)
            &&
            (ReedSolomon.verify_str r shards)
          end
          &&
          begin (* bigstr *)
            let shards = make_random_shards_bigstr size (data + parity) in
            ReedSolomon.encode_bigstr r shards;
            let expect = copy_shards_bigstr shards in

            (* corrupt shards *)
            Array.iter
              (fun p ->
                 fill_random_bigstr shards.(p)
              )
              corrupt_pos_s;
            let shard_present = Array.make (data + parity) true in
            Array.iter
              (fun p ->
                 shard_present.(p) <- false;
              )
              corrupt_pos_s;

            (* reconstruct *)
            ReedSolomon.reconstruct_bigstr r shards shard_present;

            (ReedSolomon.verify_bigstr r expect)
            &&
            (expect = shards)
            &&
            (ReedSolomon.verify_bigstr r shards)
          end
       ))

let qc_encode_verify_reconstruct_verify_opt =
  QCheck_runner.to_ounit2_test
    (QCheck.Test.make ~count:500 ~name:"qc_encode_verify_reconstruct_verify_opt"
       QCheck.(triple (int_range 1 255) (int_range 1 255) (triple pos_int (int_range 1 1000) unit))
       (fun (data, parity, (corrupt, size, ())) ->
          QCheck.assume (0 < data);
          QCheck.assume (0 < parity);
          QCheck.assume (data + parity <= 256);
          QCheck.assume (0 < size);

          let corrupt = corrupt mod (parity + 1) in

          let corrupt_pos_s = Array.make corrupt 0 in
          for i = 0 to (corrupt) - 1 do
            let pos = ref (Random.int (data + parity)) in

            while Array.mem !pos corrupt_pos_s do
              pos := Random.int (data + parity);
            done;

            corrupt_pos_s.(i) <- !pos;
          done;

          let r = ReedSolomon.make data parity in

          begin (* bytes *)
            let shards = make_random_shards_bytes size (data + parity) in
            ReedSolomon.encode_bytes r shards;
            let expect = copy_shards_bytes shards in

            let shards = RS_Shard_utils.shards_to_option_shards_bytes expect in

            (* corrupt shards *)
            Array.iter
              (fun p ->
                 shards.(p) <- None;
              )
              corrupt_pos_s;

            (* reconstruct *)
            ReedSolomon.reconstruct_opt_bytes r shards;

            let shards = RS_Shard_utils.option_shards_to_shards_bytes shards in

            (ReedSolomon.verify_bytes r expect)
            &&
            (expect = shards)
            &&
            (ReedSolomon.verify_bytes r shards)
          end
          &&
          begin (* str *)
            let shards = make_random_shards_str size (data + parity) in
            ReedSolomon.encode_str r shards;
            let expect = copy_shards_str shards in

            let shards = RS_Shard_utils.shards_to_option_shards_str expect in

            (* corrupt shards *)
            Array.iter
              (fun p ->
                 shards.(p) <- None;
              )
              corrupt_pos_s;

            (* reconstruct *)
            ReedSolomon.reconstruct_opt_str r shards;

            let shards = RS_Shard_utils.option_shards_to_shards_str shards in

            (ReedSolomon.verify_str r expect)
            &&
            (expect = shards)
            &&
            (ReedSolomon.verify_str r shards)
          end
          &&
          begin (* bigstr *)
            let shards = make_random_shards_bigstr size (data + parity) in
            ReedSolomon.encode_bigstr r shards;
            let expect = copy_shards_bigstr shards in

            let shards = RS_Shard_utils.shards_to_option_shards_bigstr expect in

            (* corrupt shards *)
            Array.iter
              (fun p ->
                 shards.(p) <- None;
              )
              corrupt_pos_s;

            (* reconstruct *)
            ReedSolomon.reconstruct_opt_bigstr r shards;

            let shards = RS_Shard_utils.option_shards_to_shards_bigstr shards in

            (ReedSolomon.verify_bigstr r expect)
            &&
            (expect = shards)
            &&
            (ReedSolomon.verify_bigstr r shards)
          end
       ))

let qc_verify =
  QCheck_runner.to_ounit2_test
    (QCheck.Test.make ~count:500 ~name:"qc_verify"
       QCheck.(triple (int_range 1 255) (int_range 1 255) (triple pos_int (int_range 1 1000) unit))
       (fun (data, parity, (corrupt, size, ())) ->
          QCheck.assume (0 < data);
          QCheck.assume (0 < parity);
          QCheck.assume (data + parity <= 256);
          QCheck.assume (0 < size);

          let corrupt = corrupt mod (parity + 1) in

          let corrupt_pos_s = Array.make corrupt 0 in
          for i = 0 to (corrupt) - 1 do
            let pos = ref (Random.int (data + parity)) in

            while Array.mem !pos corrupt_pos_s do
              pos := Random.int (data + parity);
            done;

            corrupt_pos_s.(i) <- !pos;
          done;

          let r = ReedSolomon.make data parity in

          begin (* bytes *)
            let shards = make_random_shards_bytes size (data + parity) in
            ReedSolomon.encode_bytes r shards;
            let expect = copy_shards_bytes shards in

            (* corrupt shards *)
            Array.iter
              (fun p ->
                 fill_random shards.(p)
              )
              corrupt_pos_s;

            (ReedSolomon.verify_bytes r expect)
            &&
            ((corrupt > 0 && expect <> shards)
             || (corrupt = 0 && expect = shards))
            &&
            ((corrupt > 0 && not (ReedSolomon.verify_bytes r shards))
             || (corrupt = 0 && (ReedSolomon.verify_bytes r shards)))
          end
          &&
          begin (* str *)
            let shards = make_random_shards_str size (data + parity) in
            ReedSolomon.encode_str r shards;
            let expect = copy_shards_str shards in

            (* corrupt shards *)
            Array.iter
              (fun p ->
                 fill_random (Bytes.unsafe_of_string shards.(p))
              )
              corrupt_pos_s;

            (ReedSolomon.verify_str r expect)
            &&
            ((corrupt > 0 && expect <> shards)
             || (corrupt = 0 && expect = shards))
            &&
            ((corrupt > 0 && not (ReedSolomon.verify_str r shards))
             || (corrupt = 0 && (ReedSolomon.verify_str r shards)))
          end
          &&
          begin (* bigstr *)
            let shards = make_random_shards_bigstr size (data + parity) in
            ReedSolomon.encode_bigstr r shards;
            let expect = copy_shards_bigstr shards in

            (* corrupt shards *)
            Array.iter
              (fun p ->
                 fill_random_bigstr shards.(p)
              )
              corrupt_pos_s;

            (ReedSolomon.verify_bigstr r expect)
            &&
            ((corrupt > 0 && expect <> shards)
             || (corrupt = 0 && expect = shards))
            &&
            ((corrupt > 0 && not (ReedSolomon.verify_bigstr r shards))
             || (corrupt = 0 && (ReedSolomon.verify_bigstr r shards)))
          end
       ))

let qc_encode_sep_same_as_encode =
  QCheck_runner.to_ounit2_test
    (QCheck.Test.make ~count:500 ~name:"qc_encode_sep_same_as_encode"
       QCheck.(triple (int_range 1 255) (int_range 1 255) (int_range 1 1000))
       (fun (data, parity, size) ->
          QCheck.assume (0 < data);
          QCheck.assume (0 < parity);
          QCheck.assume (data + parity <= 256);
          QCheck.assume (0 < size);

          let r = ReedSolomon.make data parity in

          begin (* bytes *)
            let shards = make_random_shards_bytes size (data + parity) in
            ReedSolomon.encode_bytes r shards;
            let expect = copy_shards_bytes shards in

            begin
              let (data, parity) = array_split_at shards data in

              ReedSolomon.encode_sep_bytes r data parity;
            end;

            expect = shards
          end
          &&
          begin (* str *)
            let shards = make_random_shards_str size (data + parity) in
            ReedSolomon.encode_str r shards;
            let expect = copy_shards_str shards in

            begin
              let (data, parity) = array_split_at shards data in

              ReedSolomon.encode_sep_str r data parity;
            end;

            expect = shards
          end
          &&
          begin (* bigstr *)
            let shards = make_random_shards_bigstr size (data + parity) in
            ReedSolomon.encode_bigstr r shards;
            let expect = copy_shards_bigstr shards in

            begin
              let (data, parity) = array_split_at shards data in

              ReedSolomon.encode_sep_bigstr r data parity;
            end;

            expect = shards
          end
       ))

let qc_encode_single_same_as_encode =
  QCheck_runner.to_ounit2_test
    (QCheck.Test.make ~count:500 ~name:"qc_encode_single_same_as_encode"
       QCheck.(triple (int_range 1 255) (int_range 1 255) (int_range 1 1000))
       (fun (data, parity, size) ->
          QCheck.assume (0 < data);
          QCheck.assume (0 < parity);
          QCheck.assume (data + parity <= 256);
          QCheck.assume (0 < size);

          let r = ReedSolomon.make data parity in

          begin (* bytes *)
            let shards = make_random_shards_bytes size (data + parity) in
            ReedSolomon.encode_bytes r shards;
            let expect = copy_shards_bytes shards in

            begin
              for i = 0 to (data) - 1 do
                ReedSolomon.encode_single_bytes r i shards;
              done
            end;

            expect = shards
          end
          &&
          begin (* str *)
            let shards = make_random_shards_str size (data + parity) in
            ReedSolomon.encode_str r shards;
            let expect = copy_shards_str shards in

            begin
              for i = 0 to (data) - 1 do
                ReedSolomon.encode_single_str r i shards;
              done
            end;

            expect = shards
          end
          &&
          begin (* bigstr *)
            let shards = make_random_shards_bigstr size (data + parity) in
            ReedSolomon.encode_bigstr r shards;
            let expect = copy_shards_bigstr shards in

            begin
              for i = 0 to (data) - 1 do
                ReedSolomon.encode_single_bigstr r i shards;
              done
            end;

            expect = shards
          end
       ))

let qc_encode_single_sep_same_as_encode =
  QCheck_runner.to_ounit2_test
    (QCheck.Test.make ~count:500 ~name:"qc_encode_single_sep_same_as_encode"
       QCheck.(triple (int_range 1 255) (int_range 1 255) (int_range 1 1000))
       (fun (data, parity, size) ->
          QCheck.assume (0 < data);
          QCheck.assume (0 < parity);
          QCheck.assume (data + parity <= 256);
          QCheck.assume (0 < size);

          let r = ReedSolomon.make data parity in

          begin (* bytes *)
            let shards = make_random_shards_bytes size (data + parity) in
            ReedSolomon.encode_bytes r shards;
            let expect = copy_shards_bytes shards in

            begin
              let (data_shards, parity_shards) = array_split_at shards data in

              for i = 0 to (data) - 1 do
                ReedSolomon.encode_single_sep_bytes r i data_shards.(i) parity_shards;
              done
            end;

            expect = shards
          end
          &&
          begin (* str *)
            let shards = make_random_shards_str size (data + parity) in
            ReedSolomon.encode_str r shards;
            let expect = copy_shards_str shards in

            begin
              let (data_shards, parity_shards) = array_split_at shards data in

              for i = 0 to (data) - 1 do
                ReedSolomon.encode_single_sep_str r i data_shards.(i) parity_shards;
              done
            end;

            expect = shards
          end
          &&
          begin (* bigstr *)
            let shards = make_random_shards_bigstr size (data + parity) in
            ReedSolomon.encode_bigstr r shards;
            let expect = copy_shards_bigstr shards in

            begin
              let (data_shards, parity_shards) = array_split_at shards data in

              for i = 0 to (data) - 1 do
                ReedSolomon.encode_single_sep_bigstr r i data_shards.(i) parity_shards;
              done
            end;

            expect = shards
          end
       ))

let test_reconstruct_error_handling test_ctxt =
  let r = ReedSolomon.make 2 2 in

  begin (* bytes *)
    let shards = [|Bytes.of_string "\000\001\002";
                   Bytes.of_string "\003\004\005";
                   Bytes.of_string "\200\201\203";
                   Bytes.of_string "\100\101\102"|] in

    ReedSolomon.encode_bytes r shards;

    begin
      shards.(0).%(0) <- 101 |> char_of_int;
      shards.(0).%(1) <- 101 |> char_of_int;
      shards.(0).%(2) <- 101 |> char_of_int;

      let shards_present = [|false; true; true; true; true|] in
      assert_equal (Error RS_Error.InvalidShardFlags) (ReedSolomon.reconstruct_bytes_no_exn r shards shards_present);

      let shards_present = [|false; true; true|] in
      assert_equal (Error RS_Error.InvalidShardFlags) (ReedSolomon.reconstruct_bytes_no_exn r shards shards_present);

      let shards_present = [|true; false; false; false|] in
      assert_equal (Error RS_Error.TooFewShardsPresent) (ReedSolomon.reconstruct_bytes_no_exn r shards shards_present);

      let shards_present = [|true; false; false; true|] in
      ReedSolomon.reconstruct_bytes r shards shards_present;
    end
  end;
  begin (* str *)
    let shards = [|"\000\001\002";
                   "\003\004\005";
                   "\200\201\203";
                   "\100\101\102"|] in

    ReedSolomon.encode_str r shards;

    begin
      (Bytes.unsafe_of_string shards.(0)).%(0) <- 101 |> char_of_int;
      (Bytes.unsafe_of_string shards.(0)).%(1) <- 101 |> char_of_int;
      (Bytes.unsafe_of_string shards.(0)).%(2) <- 101 |> char_of_int;

      let shards_present = [|false; true; true; true; true|] in
      assert_equal (Error RS_Error.InvalidShardFlags) (ReedSolomon.reconstruct_str_no_exn r shards shards_present);

      let shards_present = [|false; true; true|] in
      assert_equal (Error RS_Error.InvalidShardFlags) (ReedSolomon.reconstruct_str_no_exn r shards shards_present);

      let shards_present = [|true; false; false; false|] in
      assert_equal (Error RS_Error.TooFewShardsPresent) (ReedSolomon.reconstruct_str_no_exn r shards shards_present);

      let shards_present = [|true; false; false; true|] in
      ReedSolomon.reconstruct_str r shards shards_present;
    end
  end;
  begin (* bigstr *)
    let shards = [|Bigstring.of_string "\000\001\002";
                   Bigstring.of_string "\003\004\005";
                   Bigstring.of_string "\200\201\203";
                   Bigstring.of_string "\100\101\102"|] in

    ReedSolomon.encode_bigstr r shards;

    begin
      shards.(0).{0} <- 101 |> char_of_int;
      shards.(0).{1} <- 101 |> char_of_int;
      shards.(0).{2} <- 101 |> char_of_int;

      let shards_present = [|false; true; true; true; true|] in
      assert_equal (Error RS_Error.InvalidShardFlags) (ReedSolomon.reconstruct_bigstr_no_exn r shards shards_present);

      let shards_present = [|false; true; true|] in
      assert_equal (Error RS_Error.InvalidShardFlags) (ReedSolomon.reconstruct_bigstr_no_exn r shards shards_present);

      let shards_present = [|true; false; false; false|] in
      assert_equal (Error RS_Error.TooFewShardsPresent) (ReedSolomon.reconstruct_bigstr_no_exn r shards shards_present);

      let shards_present = [|true; false; false; true|] in
      ReedSolomon.reconstruct_bigstr r shards shards_present;
    end
  end

let test_one_encode test_ctxt =
  let r = ReedSolomon.make 5 5 in

  begin (* bytes *)
    let shards = [|Bytes.of_string "\000\001";
                   Bytes.of_string "\004\005";
                   Bytes.of_string "\002\003";
                   Bytes.of_string "\006\007";
                   Bytes.of_string "\008\009";
                   Bytes.of_string "\000\000";
                   Bytes.of_string "\000\000";
                   Bytes.of_string "\000\000";
                   Bytes.of_string "\000\000";
                   Bytes.of_string "\000\000"|] in

    ReedSolomon.encode_bytes r shards;

    ( assert_equal shards.(5).%(0) (12 |> char_of_int);
      assert_equal shards.(5).%(1) (13 |> char_of_int) );
    ( assert_equal shards.(6).%(0) (10 |> char_of_int);
      assert_equal shards.(6).%(1) (11 |> char_of_int) );
    ( assert_equal shards.(7).%(0) (14 |> char_of_int);
      assert_equal shards.(7).%(1) (15 |> char_of_int) );
    ( assert_equal shards.(8).%(0) (90 |> char_of_int);
      assert_equal shards.(8).%(1) (91 |> char_of_int) );
    ( assert_equal shards.(9).%(0) (94 |> char_of_int);
      assert_equal shards.(9).%(1) (95 |> char_of_int) );

    assert_bool "verification" (ReedSolomon.verify_bytes r shards);

    shards.(8).%(0) <- ((shards.(8).%(0) |> int_of_char) + 1) |> char_of_int;
    assert_bool "verification" (not (ReedSolomon.verify_bytes r shards))
  end;
  begin (* str *)
    let shards = [|"\000\001";
                   "\004\005";
                   "\002\003";
                   "\006\007";
                   "\008\009";
                   "\000\000";
                   "\000\000";
                   "\000\000";
                   "\000\000";
                   "\000\000"|] in

    ReedSolomon.encode_str r shards;

    ( assert_equal shards.(5).[0] (12 |> char_of_int);
      assert_equal shards.(5).[1] (13 |> char_of_int) );
    ( assert_equal shards.(6).[0] (10 |> char_of_int);
      assert_equal shards.(6).[1] (11 |> char_of_int) );
    ( assert_equal shards.(7).[0] (14 |> char_of_int);
      assert_equal shards.(7).[1] (15 |> char_of_int) );
    ( assert_equal shards.(8).[0] (90 |> char_of_int);
      assert_equal shards.(8).[1] (91 |> char_of_int) );
    ( assert_equal shards.(9).[0] (94 |> char_of_int);
      assert_equal shards.(9).[1] (95 |> char_of_int) );

    assert_bool "verification" (ReedSolomon.verify_str r shards);

    (Bytes.unsafe_of_string shards.(8)).%(0) <- ((shards.(8).[0] |> int_of_char) + 1) |> char_of_int;
    assert_bool "verification" (not (ReedSolomon.verify_str r shards))
  end;
  begin (* bigstr *)
    let shards = [|Bigstring.of_string "\000\001";
                   Bigstring.of_string "\004\005";
                   Bigstring.of_string "\002\003";
                   Bigstring.of_string "\006\007";
                   Bigstring.of_string "\008\009";
                   Bigstring.of_string "\000\000";
                   Bigstring.of_string "\000\000";
                   Bigstring.of_string "\000\000";
                   Bigstring.of_string "\000\000";
                   Bigstring.of_string "\000\000"|] in

    ReedSolomon.encode_bigstr r shards;

    ( assert_equal shards.(5).{0} (12 |> char_of_int);
      assert_equal shards.(5).{1} (13 |> char_of_int) );
    ( assert_equal shards.(6).{0} (10 |> char_of_int);
      assert_equal shards.(6).{1} (11 |> char_of_int) );
    ( assert_equal shards.(7).{0} (14 |> char_of_int);
      assert_equal shards.(7).{1} (15 |> char_of_int) );
    ( assert_equal shards.(8).{0} (90 |> char_of_int);
      assert_equal shards.(8).{1} (91 |> char_of_int) );
    ( assert_equal shards.(9).{0} (94 |> char_of_int);
      assert_equal shards.(9).{1} (95 |> char_of_int) );

    assert_bool "verification" (ReedSolomon.verify_bigstr r shards);

    shards.(8).{0} <- ((shards.(8).{0} |> int_of_char) + 1) |> char_of_int;
    assert_bool "verification" (not (ReedSolomon.verify_bigstr r shards))
  end

let test_verify_too_few_shards test_ctxt =
  let r = ReedSolomon.make 3 2 in

  begin (* bytes *)
    let shards = make_random_shards_bytes 10 4 in

    assert_equal
      (Error RS_Error.TooFewShards)
      (ReedSolomon.verify_bytes_no_exn r shards)
  end;
  begin (* str *)
    let shards = make_random_shards_str 10 4 in

    assert_equal
      (Error RS_Error.TooFewShards)
      (ReedSolomon.verify_str_no_exn r shards)
  end;
  begin (* bigstr *)
    let shards = make_random_shards_bigstr 10 4 in

    assert_equal
      (Error RS_Error.TooFewShards)
      (ReedSolomon.verify_bigstr_no_exn r shards)
  end

let test_verify_with_buffer_incorrect_buffer_sizes test_ctxt =
  let r = ReedSolomon.make 3 2 in

  begin (* bytes *)
    begin
      (* Test too few shards in buffer *)
      let shards = make_random_shards_bytes 100 5 in

      let buffer = RS_Shard_utils.make_blank_shards_bytes ~size_per_shard:100 ~count:1 in

      assert_equal
        (Error RS_Error.TooFewBufferShards)
        (ReedSolomon.verify_with_buffer_bytes_no_exn r shards buffer)
    end;
    begin
      (* Test too many shards in buffer *)
      let shards = make_random_shards_bytes 100 5 in

      let buffer = RS_Shard_utils.make_blank_shards_bytes ~size_per_shard:100 ~count:3 in

      assert_equal
        (Error RS_Error.TooManyBufferShards)
        (ReedSolomon.verify_with_buffer_bytes_no_exn r shards buffer)
    end;
    begin
      (* Test correct number of shards in buffer *)
      let shards = make_random_shards_bytes 100 5 in

      ReedSolomon.encode_bytes r shards;

      let buffer = RS_Shard_utils.make_blank_shards_bytes ~size_per_shard:100 ~count:2 in

      assert_bool "verification" (ReedSolomon.verify_with_buffer_bytes r shards buffer)
    end;
    begin
      (* Test having first buffer being empty *)
      let shards = make_random_shards_bytes 100 5 in

      let buffer = RS_Shard_utils.make_blank_shards_bytes ~size_per_shard:100 ~count:2 in
      buffer.(0) <- Bytes.create 0;

      assert_equal
        (Error RS_Error.EmptyShard)
        (ReedSolomon.verify_with_buffer_bytes_no_exn r shards buffer)
    end;
    begin
      (* Test having shards of inconsistent length in buffer *)
      let shards = make_random_shards_bytes 100 5 in

      let buffer = RS_Shard_utils.make_blank_shards_bytes ~size_per_shard:100 ~count:2 in
      buffer.(0) <- Bytes.create 99;

      assert_equal
        (Error RS_Error.IncorrectShardSize)
        (ReedSolomon.verify_with_buffer_bytes_no_exn r shards buffer)
    end
  end;
  begin (* str *)
    begin
      (* Test too few shards in buffer *)
      let shards = make_random_shards_str 100 5 in

      let buffer = RS_Shard_utils.make_blank_shards_str ~size_per_shard:100 ~count:1 in

      assert_equal
        (Error RS_Error.TooFewBufferShards)
        (ReedSolomon.verify_with_buffer_str_no_exn r shards buffer)
    end;
    begin
      (* Test too many shards in buffer *)
      let shards = make_random_shards_str 100 5 in

      let buffer = RS_Shard_utils.make_blank_shards_str ~size_per_shard:100 ~count:3 in

      assert_equal
        (Error RS_Error.TooManyBufferShards)
        (ReedSolomon.verify_with_buffer_str_no_exn r shards buffer)
    end;
    begin
      (* Test correct number of shards in buffer *)
      let shards = make_random_shards_str 100 5 in

      ReedSolomon.encode_str r shards;

      let buffer = RS_Shard_utils.make_blank_shards_str ~size_per_shard:100 ~count:2 in

      assert_bool "verification" (ReedSolomon.verify_with_buffer_str r shards buffer)
    end;
    begin
      (* Test having first buffer being empty *)
      let shards = make_random_shards_str 100 5 in

      let buffer = RS_Shard_utils.make_blank_shards_str ~size_per_shard:100 ~count:2 in
      buffer.(0) <- "";

      assert_equal
        (Error RS_Error.EmptyShard)
        (ReedSolomon.verify_with_buffer_str_no_exn r shards buffer)
    end;
    begin
      (* Test having shards of inconsistent length in buffer *)
      let shards = make_random_shards_str 100 5 in

      let buffer = RS_Shard_utils.make_blank_shards_str ~size_per_shard:100 ~count:2 in
      buffer.(0) <- String.make 99 '\000';

      assert_equal
        (Error RS_Error.IncorrectShardSize)
        (ReedSolomon.verify_with_buffer_str_no_exn r shards buffer)
    end
  end;
  begin (* bigstr *)
    begin
      (* Test too few shards in buffer *)
      let shards = make_random_shards_bigstr 100 5 in

      let buffer = RS_Shard_utils.make_blank_shards_bigstr ~size_per_shard:100 ~count:1 in

      assert_equal
        (Error RS_Error.TooFewBufferShards)
        (ReedSolomon.verify_with_buffer_bigstr_no_exn r shards buffer)
    end;
    begin
      (* Test too many shards in buffer *)
      let shards = make_random_shards_bigstr 100 5 in

      let buffer = RS_Shard_utils.make_blank_shards_bigstr ~size_per_shard:100 ~count:3 in

      assert_equal
        (Error RS_Error.TooManyBufferShards)
        (ReedSolomon.verify_with_buffer_bigstr_no_exn r shards buffer)
    end;
    begin
      (* Test correct number of shards in buffer *)
      let shards = make_random_shards_bigstr 100 5 in

      ReedSolomon.encode_bigstr r shards;

      let buffer = RS_Shard_utils.make_blank_shards_bigstr ~size_per_shard:100 ~count:2 in

      assert_bool "verification" (ReedSolomon.verify_with_buffer_bigstr r shards buffer)
    end;
    begin
      (* Test having first buffer being empty *)
      let shards = make_random_shards_bigstr 100 5 in

      let buffer = RS_Shard_utils.make_blank_shards_bigstr ~size_per_shard:100 ~count:2 in
      buffer.(0) <- Bigstring.create 0;

      assert_equal
        (Error RS_Error.EmptyShard)
        (ReedSolomon.verify_with_buffer_bigstr_no_exn r shards buffer)
    end;
    begin
      (* Test having shards of inconsistent length in buffer *)
      let shards = make_random_shards_bigstr 100 5 in

      let buffer = RS_Shard_utils.make_blank_shards_bigstr ~size_per_shard:100 ~count:2 in
      buffer.(0) <- Bigstring.create 99;

      assert_equal
        (Error RS_Error.IncorrectShardSize)
        (ReedSolomon.verify_with_buffer_bigstr_no_exn r shards buffer)
    end
  end

let test_verify_with_buffer_gives_correct_parity_shards test_ctxt =
  let r = ReedSolomon.make 10 3 in

  begin (* bytes *)
    for _ = 0 to (100) - 1 do
      let shards = make_random_shards_bigstr 100 13 in
      let shards_copy = copy_shards_bigstr shards in

      ReedSolomon.encode_bigstr r shards;

      begin
        let buffer = make_random_shards_bigstr 100 3 in

        assert_bool "verification" (not (ReedSolomon.verify_with_buffer_bigstr r shards_copy buffer));

        assert_equal (Array.sub shards 10 3) buffer;
      end;
      begin
        let buffer = make_random_shards_bigstr 100 3 in

        assert_bool "verification" (ReedSolomon.verify_with_buffer_bigstr r shards buffer);

        assert_equal (Array.sub shards 10 3) buffer;
      end
    done
  end;
  begin (* str *)
    for _ = 0 to (100) - 1 do
      let shards = make_random_shards_str 100 13 in
      let shards_copy = copy_shards_str shards in

      ReedSolomon.encode_str r shards;

      begin
        let buffer = make_random_shards_str 100 3 in

        assert_bool "verification" (not (ReedSolomon.verify_with_buffer_str r shards_copy buffer));

        assert_equal (Array.sub shards 10 3) buffer;
      end;
      begin
        let buffer = make_random_shards_str 100 3 in

        assert_bool "verification" (ReedSolomon.verify_with_buffer_str r shards buffer);

        assert_equal (Array.sub shards 10 3) buffer;
      end
    done
  end;
  begin (* bigstr *)
    for _ = 0 to (100) - 1 do
      let shards = make_random_shards_bigstr 100 13 in
      let shards_copy = copy_shards_bigstr shards in

      ReedSolomon.encode_bigstr r shards;

      begin
        let buffer = make_random_shards_bigstr 100 3 in

        assert_bool "verification" (not (ReedSolomon.verify_with_buffer_bigstr r shards_copy buffer));

        assert_equal (Array.sub shards 10 3) buffer;
      end;
      begin
        let buffer = make_random_shards_bigstr 100 3 in

        assert_bool "verification" (ReedSolomon.verify_with_buffer_bigstr r shards buffer);

        assert_equal (Array.sub shards 10 3) buffer;
      end
    done
  end

let test_slices_or_shards_count_check test_ctxt =
  let r = ReedSolomon.make 3 2 in

  begin (* bytes *)
    begin
      let shards = make_random_shards_bytes 10 4 in

      assert_equal
        (Error RS_Error.TooFewShards)
        (ReedSolomon.encode_bytes_no_exn r shards);
      assert_equal
        (Error RS_Error.TooFewShards)
        (ReedSolomon.verify_bytes_no_exn r shards);

      let option_shards = RS_Shard_utils.shards_to_option_shards_bytes shards in

      assert_equal
        (Error RS_Error.TooFewShards)
        (ReedSolomon.reconstruct_opt_bytes_no_exn r option_shards)
    end;
    begin
      let shards = make_random_shards_bytes 10 6 in

      assert_equal
        (Error RS_Error.TooManyShards)
        (ReedSolomon.encode_bytes_no_exn r shards);
      assert_equal
        (Error RS_Error.TooManyShards)
        (ReedSolomon.verify_bytes_no_exn r shards);

      let option_shards = RS_Shard_utils.shards_to_option_shards_bytes shards in

      assert_equal
        (Error RS_Error.TooManyShards)
        (ReedSolomon.reconstruct_opt_bytes_no_exn r option_shards)
    end
  end;
  begin (* str *)
    begin
      let shards = make_random_shards_str 10 4 in

      assert_equal
        (Error RS_Error.TooFewShards)
        (ReedSolomon.encode_str_no_exn r shards);
      assert_equal
        (Error RS_Error.TooFewShards)
        (ReedSolomon.verify_str_no_exn r shards);

      let option_shards = RS_Shard_utils.shards_to_option_shards_str shards in

      assert_equal
        (Error RS_Error.TooFewShards)
        (ReedSolomon.reconstruct_opt_str_no_exn r option_shards)
    end;
    begin
      let shards = make_random_shards_str 10 6 in

      assert_equal
        (Error RS_Error.TooManyShards)
        (ReedSolomon.encode_str_no_exn r shards);
      assert_equal
        (Error RS_Error.TooManyShards)
        (ReedSolomon.verify_str_no_exn r shards);

      let option_shards = RS_Shard_utils.shards_to_option_shards_str shards in

      assert_equal
        (Error RS_Error.TooManyShards)
        (ReedSolomon.reconstruct_opt_str_no_exn r option_shards)
    end
  end;
  begin (* bigstr *)
    begin
      let shards = make_random_shards_bigstr 10 4 in

      assert_equal
        (Error RS_Error.TooFewShards)
        (ReedSolomon.encode_bigstr_no_exn r shards);
      assert_equal
        (Error RS_Error.TooFewShards)
        (ReedSolomon.verify_bigstr_no_exn r shards);

      let option_shards = RS_Shard_utils.shards_to_option_shards_bigstr shards in

      assert_equal
        (Error RS_Error.TooFewShards)
        (ReedSolomon.reconstruct_opt_bigstr_no_exn r option_shards)
    end;
    begin
      let shards = make_random_shards_bigstr 10 6 in

      assert_equal
        (Error RS_Error.TooManyShards)
        (ReedSolomon.encode_bigstr_no_exn r shards);
      assert_equal
        (Error RS_Error.TooManyShards)
        (ReedSolomon.verify_bigstr_no_exn r shards);

      let option_shards = RS_Shard_utils.shards_to_option_shards_bigstr shards in

      assert_equal
        (Error RS_Error.TooManyShards)
        (ReedSolomon.reconstruct_opt_bigstr_no_exn r option_shards)
    end
  end

let test_check_slices_or_shards_size test_ctxt =
  let r = ReedSolomon.make 2 2 in

  begin (* bytes *)
    begin
      let shards = [|Bytes.of_string "\000\000\000";
                     Bytes.of_string "\000\001";
                     Bytes.of_string "\001\002\003";
                     Bytes.of_string "\000\000\000";|] in

      assert_equal
        (Error RS_Error.IncorrectShardSize)
        (ReedSolomon.encode_bytes_no_exn r shards);

      assert_equal
        (Error RS_Error.IncorrectShardSize)
        (ReedSolomon.verify_bytes_no_exn r shards);

      let option_shards = RS_Shard_utils.shards_to_option_shards_bytes shards in

      assert_equal
        (Error RS_Error.IncorrectShardSize)
        (ReedSolomon.reconstruct_opt_bytes_no_exn r option_shards)
    end;
    begin
      let shards = [|Bytes.of_string "\000\001";
                     Bytes.of_string "\000\001";
                     Bytes.of_string "\001\002\003";
                     Bytes.of_string "\000\000\000";|] in

      assert_equal
        (Error RS_Error.IncorrectShardSize)
        (ReedSolomon.encode_bytes_no_exn r shards);

      assert_equal
        (Error RS_Error.IncorrectShardSize)
        (ReedSolomon.verify_bytes_no_exn r shards);

      let option_shards = RS_Shard_utils.shards_to_option_shards_bytes shards in

      assert_equal
        (Error RS_Error.IncorrectShardSize)
        (ReedSolomon.reconstruct_opt_bytes_no_exn r option_shards)
    end;
    begin
      let shards = [|Bytes.of_string "\000\001";
                     Bytes.of_string "\000\001\004";
                     Bytes.of_string "\001\002\003";
                     Bytes.of_string "\000\000\000";|] in

      assert_equal
        (Error RS_Error.IncorrectShardSize)
        (ReedSolomon.encode_bytes_no_exn r shards);

      assert_equal
        (Error RS_Error.IncorrectShardSize)
        (ReedSolomon.verify_bytes_no_exn r shards);

      let option_shards = RS_Shard_utils.shards_to_option_shards_bytes shards in

      assert_equal
        (Error RS_Error.IncorrectShardSize)
        (ReedSolomon.reconstruct_opt_bytes_no_exn r option_shards)
    end;
    begin
      let shards = [|Bytes.of_string "";
                     Bytes.of_string "\000\001\003";
                     Bytes.of_string "\001\002\003";
                     Bytes.of_string "\000\000\000";|] in

      assert_equal
        (Error RS_Error.EmptyShard)
        (ReedSolomon.encode_bytes_no_exn r shards);

      assert_equal
        (Error RS_Error.EmptyShard)
        (ReedSolomon.verify_bytes_no_exn r shards);

      let option_shards = RS_Shard_utils.shards_to_option_shards_bytes shards in

      assert_equal
        (Error RS_Error.EmptyShard)
        (ReedSolomon.reconstruct_opt_bytes_no_exn r option_shards)
    end;
    begin
      let option_shards = [|None; None; None; None|] in

      assert_equal
        (Error RS_Error.TooFewShardsPresent)
        (ReedSolomon.reconstruct_opt_bytes_no_exn r option_shards)
    end
  end;
  begin (* str *)
    begin
      let shards = [|"\000\000\000";
                     "\000\001";
                     "\001\002\003";
                     "\000\000\000";|] in

      assert_equal
        (Error RS_Error.IncorrectShardSize)
        (ReedSolomon.encode_str_no_exn r shards);

      assert_equal
        (Error RS_Error.IncorrectShardSize)
        (ReedSolomon.verify_str_no_exn r shards);

      let option_shards = RS_Shard_utils.shards_to_option_shards_str shards in

      assert_equal
        (Error RS_Error.IncorrectShardSize)
        (ReedSolomon.reconstruct_opt_str_no_exn r option_shards)
    end;
    begin
      let shards = [|"\000\001";
                     "\000\001";
                     "\001\002\003";
                     "\000\000\000";|] in

      assert_equal
        (Error RS_Error.IncorrectShardSize)
        (ReedSolomon.encode_str_no_exn r shards);

      assert_equal
        (Error RS_Error.IncorrectShardSize)
        (ReedSolomon.verify_str_no_exn r shards);

      let option_shards = RS_Shard_utils.shards_to_option_shards_str shards in

      assert_equal
        (Error RS_Error.IncorrectShardSize)
        (ReedSolomon.reconstruct_opt_str_no_exn r option_shards)
    end;
    begin
      let shards = [|"\000\001";
                     "\000\001\004";
                     "\001\002\003";
                     "\000\000\000";|] in

      assert_equal
        (Error RS_Error.IncorrectShardSize)
        (ReedSolomon.encode_str_no_exn r shards);

      assert_equal
        (Error RS_Error.IncorrectShardSize)
        (ReedSolomon.verify_str_no_exn r shards);

      let option_shards = RS_Shard_utils.shards_to_option_shards_str shards in

      assert_equal
        (Error RS_Error.IncorrectShardSize)
        (ReedSolomon.reconstruct_opt_str_no_exn r option_shards)
    end;
    begin
      let shards = [|"";
                     "\000\001\003";
                     "\001\002\003";
                     "\000\000\000";|] in

      assert_equal
        (Error RS_Error.EmptyShard)
        (ReedSolomon.encode_str_no_exn r shards);

      assert_equal
        (Error RS_Error.EmptyShard)
        (ReedSolomon.verify_str_no_exn r shards);

      let option_shards = RS_Shard_utils.shards_to_option_shards_str shards in

      assert_equal
        (Error RS_Error.EmptyShard)
        (ReedSolomon.reconstruct_opt_str_no_exn r option_shards)
    end;
    begin
      let option_shards = [|None; None; None; None|] in

      assert_equal
        (Error RS_Error.TooFewShardsPresent)
        (ReedSolomon.reconstruct_opt_str_no_exn r option_shards)
    end
  end;
  begin (* bigstr *)
    begin
      let shards = [|Bigstring.of_string "\000\000\000";
                     Bigstring.of_string "\000\001";
                     Bigstring.of_string "\001\002\003";
                     Bigstring.of_string "\000\000\000";|] in

      assert_equal
        (Error RS_Error.IncorrectShardSize)
        (ReedSolomon.encode_bigstr_no_exn r shards);

      assert_equal
        (Error RS_Error.IncorrectShardSize)
        (ReedSolomon.verify_bigstr_no_exn r shards);

      let option_shards = RS_Shard_utils.shards_to_option_shards_bigstr shards in

      assert_equal
        (Error RS_Error.IncorrectShardSize)
        (ReedSolomon.reconstruct_opt_bigstr_no_exn r option_shards)
    end;
    begin
      let shards = [|Bigstring.of_string "\000\001";
                     Bigstring.of_string "\000\001";
                     Bigstring.of_string "\001\002\003";
                     Bigstring.of_string "\000\000\000";|] in

      assert_equal
        (Error RS_Error.IncorrectShardSize)
        (ReedSolomon.encode_bigstr_no_exn r shards);

      assert_equal
        (Error RS_Error.IncorrectShardSize)
        (ReedSolomon.verify_bigstr_no_exn r shards);

      let option_shards = RS_Shard_utils.shards_to_option_shards_bigstr shards in

      assert_equal
        (Error RS_Error.IncorrectShardSize)
        (ReedSolomon.reconstruct_opt_bigstr_no_exn r option_shards)
    end;
    begin
      let shards = [|Bigstring.of_string "\000\001";
                     Bigstring.of_string "\000\001\004";
                     Bigstring.of_string "\001\002\003";
                     Bigstring.of_string "\000\000\000";|] in

      assert_equal
        (Error RS_Error.IncorrectShardSize)
        (ReedSolomon.encode_bigstr_no_exn r shards);

      assert_equal
        (Error RS_Error.IncorrectShardSize)
        (ReedSolomon.verify_bigstr_no_exn r shards);

      let option_shards = RS_Shard_utils.shards_to_option_shards_bigstr shards in

      assert_equal
        (Error RS_Error.IncorrectShardSize)
        (ReedSolomon.reconstruct_opt_bigstr_no_exn r option_shards)
    end;
    begin
      let shards = [|Bigstring.of_string "";
                     Bigstring.of_string "\000\001\003";
                     Bigstring.of_string "\001\002\003";
                     Bigstring.of_string "\000\000\000";|] in

      assert_equal
        (Error RS_Error.EmptyShard)
        (ReedSolomon.encode_bigstr_no_exn r shards);

      assert_equal
        (Error RS_Error.EmptyShard)
        (ReedSolomon.verify_bigstr_no_exn r shards);

      let option_shards = RS_Shard_utils.shards_to_option_shards_bigstr shards in

      assert_equal
        (Error RS_Error.EmptyShard)
        (ReedSolomon.reconstruct_opt_bigstr_no_exn r option_shards)
    end;
    begin
      let option_shards = [|None; None; None; None|] in

      assert_equal
        (Error RS_Error.TooFewShardsPresent)
        (ReedSolomon.reconstruct_opt_bigstr_no_exn r option_shards)
    end
  end

let shardbyshard_encode_correctly test_ctxt =
  begin (* bytes *)
    let r = ReedSolomon.make 10 3 in
    let sbs = ShardByShard.make r in

    let shards = make_random_shards_bytes 10_000 13 in
    let shards_copy = copy_shards_bytes shards in

    ReedSolomon.encode_bytes r shards;

    for i = 0 to (10) - 1 do
      assert_equal i (ShardByShard.cur_input_index sbs);

      ShardByShard.encode_bytes sbs shards_copy
    done;

    assert_bool "verification" (ShardByShard.parity_ready sbs);

    assert_equal shards shards_copy;

    ShardByShard.reset_force sbs;

    assert_equal 0 (ShardByShard.cur_input_index sbs);
  end;
  begin (* str *)
    let r = ReedSolomon.make 10 3 in
    let sbs = ShardByShard.make r in

    let shards = make_random_shards_str 10_000 13 in
    let shards_copy = copy_shards_str shards in

    ReedSolomon.encode_str r shards;

    for i = 0 to (10) - 1 do
      assert_equal i (ShardByShard.cur_input_index sbs);

      ShardByShard.encode_str sbs shards_copy
    done;

    assert_bool "verification" (ShardByShard.parity_ready sbs);

    assert_equal shards shards_copy;

    ShardByShard.reset_force sbs;

    assert_equal 0 (ShardByShard.cur_input_index sbs);
  end;
  begin (* bigstr *)
    let r = ReedSolomon.make 10 3 in
    let sbs = ShardByShard.make r in

    let shards = make_random_shards_bigstr 10_000 13 in
    let shards_copy = copy_shards_bigstr shards in

    ReedSolomon.encode_bigstr r shards;

    for i = 0 to (10) - 1 do
      assert_equal i (ShardByShard.cur_input_index sbs);

      ShardByShard.encode_bigstr sbs shards_copy
    done;

    assert_bool "verification" (ShardByShard.parity_ready sbs);

    assert_equal shards shards_copy;

    ShardByShard.reset_force sbs;

    assert_equal 0 (ShardByShard.cur_input_index sbs);
  end

let qc_shardbyshard_encode_same_as_encode =
  QCheck_runner.to_ounit2_test
    (QCheck.Test.make ~count:500 ~name:"qc_shardbyshard_encode_same_as_encode"
       QCheck.(triple (int_range 1 255) (int_range 1 255) (triple (int_range 0 10) (int_range 1 1000) unit))
       (fun (data, parity, (reuse, size, ())) ->
          QCheck.assume (0 < data);
          QCheck.assume (0 < parity);
          QCheck.assume (data + parity <= 256);
          QCheck.assume (0 < size);

          let r = ReedSolomon.make data parity in
          let sbs = ShardByShard.make r in

          begin (* bytes *)
            let expect = make_random_shards_bytes size (data + parity) in
            let shards = copy_shards_bytes expect in

            let result = ref true in

            for _ = 0 to reuse do
              ReedSolomon.encode_bytes r expect;

              for i = 0 to (data) - 1 do
                assert_equal i (ShardByShard.cur_input_index sbs);

                ShardByShard.encode_bytes sbs shards
              done;

              if not (expect = shards
                      && ShardByShard.parity_ready sbs
                      && ShardByShard.cur_input_index sbs = data
                      && (ShardByShard.reset sbs;
                          (not (ShardByShard.parity_ready sbs))
                          && (ShardByShard.cur_input_index sbs = 0))) then
                result := false
            done;

            !result
          end
          &&
          begin (* str *)
            let expect = make_random_shards_str size (data + parity) in
            let shards = copy_shards_str expect in

            let result = ref true in

            for _ = 0 to reuse do
              ReedSolomon.encode_str r expect;

              for i = 0 to (data) - 1 do
                assert_equal i (ShardByShard.cur_input_index sbs);

                ShardByShard.encode_str sbs shards
              done;

              if not (expect = shards
                      && ShardByShard.parity_ready sbs
                      && ShardByShard.cur_input_index sbs = data
                      && (ShardByShard.reset sbs;
                          (not (ShardByShard.parity_ready sbs))
                          && (ShardByShard.cur_input_index sbs = 0))) then
                result := false
            done;

            !result
          end
          &&
          begin (* bigstr *)
            let expect = make_random_shards_bigstr size (data + parity) in
            let shards = copy_shards_bigstr expect in

            let result = ref true in

            for _ = 0 to reuse do
              ReedSolomon.encode_bigstr r expect;

              for i = 0 to (data) - 1 do
                assert_equal i (ShardByShard.cur_input_index sbs);

                ShardByShard.encode_bigstr sbs shards
              done;

              if not (expect = shards
                      && ShardByShard.parity_ready sbs
                      && ShardByShard.cur_input_index sbs = data
                      && (ShardByShard.reset sbs;
                          (not (ShardByShard.parity_ready sbs))
                          && (ShardByShard.cur_input_index sbs = 0))) then
                result := false
            done;

            !result
          end
       ))

let shardbyshard_encode_sep_correctly test_ctxt =
  let r = ReedSolomon.make 10 3 in
  let sbs = ShardByShard.make r in

  begin (* bytes *)
    let shards = make_random_shards_bytes 10_000 13 in
    let shards_copy = copy_shards_bytes shards in

    let (data, parity) = array_split_at shards 10 in
    let (data_copy, parity_copy) = array_split_at shards_copy 10 in

    ReedSolomon.encode_sep_bytes r data parity;

    for i = 0 to (10) - 1 do
      assert_equal i (ShardByShard.cur_input_index sbs);

      ShardByShard.encode_sep_bytes sbs data_copy parity_copy;
    done;

    assert_bool "parity ready" (ShardByShard.parity_ready sbs);

    assert_equal parity parity_copy;

    ShardByShard.reset_force sbs;

    assert_equal 0 (ShardByShard.cur_input_index sbs)
  end;
  begin (* str *)
    let shards = make_random_shards_str 10_000 13 in
    let shards_copy = copy_shards_str shards in

    let (data, parity) = array_split_at shards 10 in
    let (data_copy, parity_copy) = array_split_at shards_copy 10 in

    ReedSolomon.encode_sep_str r data parity;

    for i = 0 to (10) - 1 do
      assert_equal i (ShardByShard.cur_input_index sbs);

      ShardByShard.encode_sep_str sbs data_copy parity_copy;
    done;

    assert_bool "parity ready" (ShardByShard.parity_ready sbs);

    assert_equal parity parity_copy;

    ShardByShard.reset_force sbs;

    assert_equal 0 (ShardByShard.cur_input_index sbs)
  end;
  begin (* bigstr *)
    let shards = make_random_shards_bigstr 10_000 13 in
    let shards_copy = copy_shards_bigstr shards in

    let (data, parity) = array_split_at shards 10 in
    let (data_copy, parity_copy) = array_split_at shards_copy 10 in

    ReedSolomon.encode_sep_bigstr r data parity;

    for i = 0 to (10) - 1 do
      assert_equal i (ShardByShard.cur_input_index sbs);

      ShardByShard.encode_sep_bigstr sbs data_copy parity_copy;
    done;

    assert_bool "parity ready" (ShardByShard.parity_ready sbs);

    assert_equal parity parity_copy;

    ShardByShard.reset_force sbs;

    assert_equal 0 (ShardByShard.cur_input_index sbs)
  end

let qc_shardbyshard_encode_sep_same_as_encode =
  QCheck_runner.to_ounit2_test
    (QCheck.Test.make ~count:500 ~name:"qc_shardbyshard_encode_sep_same_as_encode"
       QCheck.(triple (int_range 1 255) (int_range 1 255) (triple (int_range 0 10) (int_range 1 1000) unit))
       (fun (data, parity, (reuse, size, ())) ->
          QCheck.assume (0 < data);
          QCheck.assume (0 < parity);
          QCheck.assume (data + parity <= 256);
          QCheck.assume (0 < size);

          let r = ReedSolomon.make data parity in
          let sbs = ShardByShard.make r in

          begin (* bytes *)
            let expect = make_random_shards_bytes size (data + parity) in
            let shards = copy_shards_bytes expect in

            let result = ref true in

            for _ = 0 to reuse do
              begin
                let (data, parity) = array_split_at expect data in
                ReedSolomon.encode_sep_bytes r data parity
              end;
              begin
                let (data_shards, parity_shards) = array_split_at shards data in

                for i = 0 to (data) - 1 do
                  assert_equal i (ShardByShard.cur_input_index sbs);

                  ShardByShard.encode_sep_bytes sbs data_shards parity_shards;
                done
              end;

              if not (expect = shards
                      && ShardByShard.parity_ready sbs
                      && ShardByShard.cur_input_index sbs = data
                      && (ShardByShard.reset sbs;
                          (not (ShardByShard.parity_ready sbs))
                          && ShardByShard.cur_input_index sbs = 0)) then
                result := false
            done;

            !result
          end
          &&
          begin (* str *)
            let expect = make_random_shards_str size (data + parity) in
            let shards = copy_shards_str expect in

            let result = ref true in

            for _ = 0 to reuse do
              begin
                let (data, parity) = array_split_at expect data in
                ReedSolomon.encode_sep_str r data parity
              end;
              begin
                let (data_shards, parity_shards) = array_split_at shards data in

                for i = 0 to (data) - 1 do
                  assert_equal i (ShardByShard.cur_input_index sbs);

                  ShardByShard.encode_sep_str sbs data_shards parity_shards;
                done
              end;

              if not (expect = shards
                      && ShardByShard.parity_ready sbs
                      && ShardByShard.cur_input_index sbs = data
                      && (ShardByShard.reset sbs;
                          (not (ShardByShard.parity_ready sbs))
                          && ShardByShard.cur_input_index sbs = 0)) then
                result := false
            done;

            !result
          end
          &&
          begin (* bigstr *)
            let expect = make_random_shards_bigstr size (data + parity) in
            let shards = copy_shards_bigstr expect in

            let result = ref true in

            for _ = 0 to reuse do
              begin
                let (data, parity) = array_split_at expect data in
                ReedSolomon.encode_sep_bigstr r data parity
              end;
              begin
                let (data_shards, parity_shards) = array_split_at shards data in

                for i = 0 to (data) - 1 do
                  assert_equal i (ShardByShard.cur_input_index sbs);

                  ShardByShard.encode_sep_bigstr sbs data_shards parity_shards;
                done
              end;

              if not (expect = shards
                      && ShardByShard.parity_ready sbs
                      && ShardByShard.cur_input_index sbs = data
                      && (ShardByShard.reset sbs;
                          (not (ShardByShard.parity_ready sbs))
                          && ShardByShard.cur_input_index sbs = 0)) then
                result := false
            done;

            !result
          end
       ))

let shardbyshard_encode_correctly_more_rigorous test_ctxt =
  begin (* bytes *)
    let r = ReedSolomon.make 10 3 in
    let sbs = ShardByShard.make r in

    let shards = make_random_shards_bytes 10_000 13 in
    let shards_copy = make_random_shards_bytes 10_000 13 in

    ReedSolomon.encode_bytes r shards;

    for i = 0 to (10) - 1 do
      assert_equal i (ShardByShard.cur_input_index sbs);

      shards_copy.(i) <- Bytes.copy shards.(i);
      ShardByShard.encode_bytes sbs shards_copy;
      fill_random shards_copy.(i);
    done;

    assert_bool "parity ready" (ShardByShard.parity_ready sbs);

    for i = 0 to (10) - 1 do
      shards_copy.(i) <- Bytes.copy shards.(i);
    done;

    assert_equal shards shards_copy;

    ShardByShard.reset_force sbs;

    assert_equal 0 (ShardByShard.cur_input_index sbs)
  end;
  begin (* str *)
    let r = ReedSolomon.make 10 3 in
    let sbs = ShardByShard.make r in

    let shards = make_random_shards_str 10_000 13 in
    let shards_copy = make_random_shards_str 10_000 13 in

    ReedSolomon.encode_str r shards;

    for i = 0 to (10) - 1 do
      assert_equal i (ShardByShard.cur_input_index sbs);

      shards_copy.(i) <- shards.(i);
      ShardByShard.encode_str sbs shards_copy;
      fill_random (Bytes.unsafe_of_string shards_copy.(i));
    done;

    assert_bool "parity ready" (ShardByShard.parity_ready sbs);

    for i = 0 to (10) - 1 do
      shards_copy.(i) <- shards.(i);
    done;

    assert_equal shards shards_copy;

    ShardByShard.reset_force sbs;

    assert_equal 0 (ShardByShard.cur_input_index sbs)
  end;
  begin (* bigstr *)
    let r = ReedSolomon.make 10 3 in
    let sbs = ShardByShard.make r in

    let shards = make_random_shards_bigstr 10_000 13 in
    let shards_copy = make_random_shards_bigstr 10_000 13 in

    ReedSolomon.encode_bigstr r shards;

    for i = 0 to (10) - 1 do
      assert_equal i (ShardByShard.cur_input_index sbs);

      shards_copy.(i) <- copy_bigstr shards.(i);
      ShardByShard.encode_bigstr sbs shards_copy;
      fill_random_bigstr shards_copy.(i);
    done;

    assert_bool "parity ready" (ShardByShard.parity_ready sbs);

    for i = 0 to (10) - 1 do
      shards_copy.(i) <- copy_bigstr shards.(i);
    done;

    assert_equal shards shards_copy;

    ShardByShard.reset_force sbs;

    assert_equal 0 (ShardByShard.cur_input_index sbs)
  end

let shardbyshard_encode_error_handling test_ctxt =
  begin (* bytes *)
    begin
      let r = ReedSolomon.make 10 3 in
      let sbs = ShardByShard.make r in

      let shards = make_random_shards_bytes 10_000 13 in

      for i = 0 to (10) - 1 do
        assert_equal i (ShardByShard.cur_input_index sbs);

        ShardByShard.encode_bytes sbs shards
      done;

      assert_bool "parity ready" (ShardByShard.parity_ready sbs);

      assert_equal
        (Error RS_SBS_Error.TooManyCalls)
        (ShardByShard.encode_bytes_no_exn sbs shards);

      ShardByShard.reset sbs;

      for i = 0 to (1) - 1 do
        assert_equal i (ShardByShard.cur_input_index sbs);

        ShardByShard.encode_bytes sbs shards
      done;

      assert_equal
        (Error RS_SBS_Error.LeftoverShards)
        (ShardByShard.reset_no_exn sbs);

      ShardByShard.reset_force sbs;

      assert_equal 0 (ShardByShard.cur_input_index sbs)
    end;
    begin
      let r = ReedSolomon.make 10 3 in
      let sbs = ShardByShard.make r in

      let shards = make_random_shards_bytes 100 13 in
      shards.(0) <- Bytes.empty;
      begin
        assert_equal 0 (ShardByShard.cur_input_index sbs);

        assert_equal
          (Error (RS_SBS_Error.RSError RS_Error.EmptyShard))
          (ShardByShard.encode_bytes_no_exn sbs shards);

        assert_equal 0 (ShardByShard.cur_input_index sbs);

        assert_equal
          (Error (RS_SBS_Error.RSError RS_Error.EmptyShard))
          (ShardByShard.encode_bytes_no_exn sbs shards);

        assert_equal 0 (ShardByShard.cur_input_index sbs);
      end;

      shards.(0) <- Bytes.make 100 '\000';

      ShardByShard.encode_bytes sbs shards;

      assert_equal 1 (ShardByShard.cur_input_index sbs)
    end;
    begin
      let r = ReedSolomon.make 10 3 in
      let sbs = ShardByShard.make r in

      let shards = make_random_shards_bytes 100 13 in
      shards.(1) <- Bytes.make 99 '\000';
      begin
        assert_equal 0 (ShardByShard.cur_input_index sbs);

        assert_equal
          (Error (RS_SBS_Error.RSError RS_Error.IncorrectShardSize))
          (ShardByShard.encode_bytes_no_exn sbs shards);

        assert_equal 0 (ShardByShard.cur_input_index sbs);

        assert_equal
          (Error (RS_SBS_Error.RSError RS_Error.IncorrectShardSize))
          (ShardByShard.encode_bytes_no_exn sbs shards);

        assert_equal 0 (ShardByShard.cur_input_index sbs);
      end;

      shards.(1) <- Bytes.make 100 '\000';

      ShardByShard.encode_bytes sbs shards;

      assert_equal 1 (ShardByShard.cur_input_index sbs);
    end
  end;
  begin (* str *)
    begin
      let r = ReedSolomon.make 10 3 in
      let sbs = ShardByShard.make r in

      let shards = make_random_shards_str 10_000 13 in

      for i = 0 to (10) - 1 do
        assert_equal i (ShardByShard.cur_input_index sbs);

        ShardByShard.encode_str sbs shards
      done;

      assert_bool "parity ready" (ShardByShard.parity_ready sbs);

      assert_equal
        (Error RS_SBS_Error.TooManyCalls)
        (ShardByShard.encode_str_no_exn sbs shards);

      ShardByShard.reset sbs;

      for i = 0 to (1) - 1 do
        assert_equal i (ShardByShard.cur_input_index sbs);

        ShardByShard.encode_str sbs shards
      done;

      assert_equal
        (Error RS_SBS_Error.LeftoverShards)
        (ShardByShard.reset_no_exn sbs);

      ShardByShard.reset_force sbs;

      assert_equal 0 (ShardByShard.cur_input_index sbs)
    end;
    begin
      let r = ReedSolomon.make 10 3 in
      let sbs = ShardByShard.make r in

      let shards = make_random_shards_str 100 13 in
      shards.(0) <- "";
      begin
        assert_equal 0 (ShardByShard.cur_input_index sbs);

        assert_equal
          (Error (RS_SBS_Error.RSError RS_Error.EmptyShard))
          (ShardByShard.encode_str_no_exn sbs shards);

        assert_equal 0 (ShardByShard.cur_input_index sbs);

        assert_equal
          (Error (RS_SBS_Error.RSError RS_Error.EmptyShard))
          (ShardByShard.encode_str_no_exn sbs shards);

        assert_equal 0 (ShardByShard.cur_input_index sbs);
      end;

      shards.(0) <- String.make 100 '\000';

      ShardByShard.encode_str sbs shards;

      assert_equal 1 (ShardByShard.cur_input_index sbs)
    end;
    begin
      let r = ReedSolomon.make 10 3 in
      let sbs = ShardByShard.make r in

      let shards = make_random_shards_str 100 13 in
      shards.(1) <- String.make 99 '\000';
      begin
        assert_equal 0 (ShardByShard.cur_input_index sbs);

        assert_equal
          (Error (RS_SBS_Error.RSError RS_Error.IncorrectShardSize))
          (ShardByShard.encode_str_no_exn sbs shards);

        assert_equal 0 (ShardByShard.cur_input_index sbs);

        assert_equal
          (Error (RS_SBS_Error.RSError RS_Error.IncorrectShardSize))
          (ShardByShard.encode_str_no_exn sbs shards);

        assert_equal 0 (ShardByShard.cur_input_index sbs);
      end;

      shards.(1) <- String.make 100 '\000';

      ShardByShard.encode_str sbs shards;

      assert_equal 1 (ShardByShard.cur_input_index sbs);
    end
  end;
  begin (* bigstr *)
    begin
      let r = ReedSolomon.make 10 3 in
      let sbs = ShardByShard.make r in

      let shards = make_random_shards_bigstr 10_000 13 in

      for i = 0 to (10) - 1 do
        assert_equal i (ShardByShard.cur_input_index sbs);

        ShardByShard.encode_bigstr sbs shards
      done;

      assert_bool "parity ready" (ShardByShard.parity_ready sbs);

      assert_equal
        (Error RS_SBS_Error.TooManyCalls)
        (ShardByShard.encode_bigstr_no_exn sbs shards);

      ShardByShard.reset sbs;

      for i = 0 to (1) - 1 do
        assert_equal i (ShardByShard.cur_input_index sbs);

        ShardByShard.encode_bigstr sbs shards
      done;

      assert_equal
        (Error RS_SBS_Error.LeftoverShards)
        (ShardByShard.reset_no_exn sbs);

      ShardByShard.reset_force sbs;

      assert_equal 0 (ShardByShard.cur_input_index sbs)
    end;
    begin
      let r = ReedSolomon.make 10 3 in
      let sbs = ShardByShard.make r in

      let shards = make_random_shards_bigstr 100 13 in
      shards.(0) <- Bigstring.create 0;
      begin
        assert_equal 0 (ShardByShard.cur_input_index sbs);

        assert_equal
          (Error (RS_SBS_Error.RSError RS_Error.EmptyShard))
          (ShardByShard.encode_bigstr_no_exn sbs shards);

        assert_equal 0 (ShardByShard.cur_input_index sbs);

        assert_equal
          (Error (RS_SBS_Error.RSError RS_Error.EmptyShard))
          (ShardByShard.encode_bigstr_no_exn sbs shards);

        assert_equal 0 (ShardByShard.cur_input_index sbs);
      end;

      shards.(0) <- Bigstring.create 100;

      ShardByShard.encode_bigstr sbs shards;

      assert_equal 1 (ShardByShard.cur_input_index sbs)
    end;
    begin
      let r = ReedSolomon.make 10 3 in
      let sbs = ShardByShard.make r in

      let shards = make_random_shards_bigstr 100 13 in
      shards.(1) <- Bigstring.create 99;
      begin
        assert_equal 0 (ShardByShard.cur_input_index sbs);

        assert_equal
          (Error (RS_SBS_Error.RSError RS_Error.IncorrectShardSize))
          (ShardByShard.encode_bigstr_no_exn sbs shards);

        assert_equal 0 (ShardByShard.cur_input_index sbs);

        assert_equal
          (Error (RS_SBS_Error.RSError RS_Error.IncorrectShardSize))
          (ShardByShard.encode_bigstr_no_exn sbs shards);

        assert_equal 0 (ShardByShard.cur_input_index sbs);
      end;

      shards.(1) <- Bigstring.create 100;

      ShardByShard.encode_bigstr sbs shards;

      assert_equal 1 (ShardByShard.cur_input_index sbs);
    end
  end

let shardbyshard_encode_sep_error_handling test_ctxt =
  begin (* bytes *)
    begin
      let r = ReedSolomon.make 10 3 in
      let sbs = ShardByShard.make r in

      let shards = make_random_shards_bytes 10_000 13 in

      let (data, parity) = array_split_at shards 10 in

      for i = 0 to (10) - 1 do
        assert_equal i (ShardByShard.cur_input_index sbs);

        ShardByShard.encode_sep_bytes sbs data parity;
      done;

      assert_bool "parity ready" (ShardByShard.parity_ready sbs);

      assert_equal
        (Error RS_SBS_Error.TooManyCalls)
        (ShardByShard.encode_sep_bytes_no_exn sbs data parity);

      ShardByShard.reset sbs;

      for i = 0 to (1) - 1 do
        assert_equal i (ShardByShard.cur_input_index sbs);

        ShardByShard.encode_sep_bytes sbs data parity
      done;

      assert_equal
        (Error RS_SBS_Error.LeftoverShards)
        (ShardByShard.reset_no_exn sbs);

      ShardByShard.reset_force sbs;

      assert_equal 0 (ShardByShard.cur_input_index sbs);
    end;
    begin
      let r = ReedSolomon.make 10 3 in

      begin
        let sbs = ShardByShard.make r in
        let shards = make_random_shards_bytes 100 13 in
        shards.(0) <- Bytes.empty;

        begin
          let (data, parity) = array_split_at shards 10 in

          assert_equal 0 (ShardByShard.cur_input_index sbs);

          assert_equal
            (Error (RS_SBS_Error.RSError RS_Error.EmptyShard))
            (ShardByShard.encode_sep_bytes_no_exn sbs data parity);

          assert_equal 0 (ShardByShard.cur_input_index sbs);

          assert_equal
            (Error (RS_SBS_Error.RSError RS_Error.EmptyShard))
            (ShardByShard.encode_sep_bytes_no_exn sbs data parity);

          assert_equal 0 (ShardByShard.cur_input_index sbs);
        end;

        shards.(0) <- Bytes.make 100 '\000';

        let (data, parity) = array_split_at shards 10 in

        ShardByShard.encode_sep_bytes sbs data parity;

        assert_equal 1 (ShardByShard.cur_input_index sbs);
      end;
      begin
        let sbs = ShardByShard.make r in

        let shards = make_random_shards_bytes 100 13 in
        shards.(10) <- Bytes.empty;

        begin
          let (data, parity) = array_split_at shards 10 in

          assert_equal 0 (ShardByShard.cur_input_index sbs);

          assert_equal
            (Error (RS_SBS_Error.RSError RS_Error.EmptyShard))
            (ShardByShard.encode_sep_bytes_no_exn sbs data parity);

          assert_equal 0 (ShardByShard.cur_input_index sbs);

          assert_equal
            (Error (RS_SBS_Error.RSError RS_Error.EmptyShard))
            (ShardByShard.encode_sep_bytes_no_exn sbs data parity);

          assert_equal 0 (ShardByShard.cur_input_index sbs);
        end;

        shards.(10) <- Bytes.make 100 '\000';

        let (data, parity) = array_split_at shards 10 in

        ShardByShard.encode_sep_bytes sbs data parity;

        assert_equal 1 (ShardByShard.cur_input_index sbs);
      end
    end;
    begin
      let r = ReedSolomon.make 10 3 in
      begin
        let sbs = ShardByShard.make r in

        let shards = make_random_shards_bytes 100 13 in
        shards.(1) <- Bytes.make 99 '\000';
        begin
          let (data, parity) = array_split_at shards 10 in

          assert_equal 0 (ShardByShard.cur_input_index sbs);

          assert_equal
            (Error (RS_SBS_Error.RSError RS_Error.IncorrectShardSize))
            (ShardByShard.encode_sep_bytes_no_exn sbs data parity);

          assert_equal 0 (ShardByShard.cur_input_index sbs);

          assert_equal
            (Error (RS_SBS_Error.RSError RS_Error.IncorrectShardSize))
            (ShardByShard.encode_sep_bytes_no_exn sbs data parity);

          assert_equal 0 (ShardByShard.cur_input_index sbs);
        end;

        shards.(1) <- Bytes.make 100 '\000';

        let (data, parity) = array_split_at shards 10 in

        ShardByShard.encode_sep_bytes sbs data parity;

        assert_equal 1 (ShardByShard.cur_input_index sbs)
      end;
      begin
        let sbs = ShardByShard.make r in

        let shards = make_random_shards_bytes 100 13 in
        shards.(11) <- Bytes.make 99 '\000';
        begin
          let (data, parity) = array_split_at shards 10 in

          assert_equal 0 (ShardByShard.cur_input_index sbs);

          assert_equal
            (Error (RS_SBS_Error.RSError (RS_Error.IncorrectShardSize)))
            (ShardByShard.encode_sep_bytes_no_exn sbs data parity);

          assert_equal 0 (ShardByShard.cur_input_index sbs);

          assert_equal
            (Error (RS_SBS_Error.RSError (RS_Error.IncorrectShardSize)))
            (ShardByShard.encode_sep_bytes_no_exn sbs data parity);

          assert_equal 0 (ShardByShard.cur_input_index sbs);
        end;

        shards.(11) <- Bytes.make 100 '\000';

        let (data, parity) = array_split_at shards 10 in

        ShardByShard.encode_sep_bytes sbs data parity;

        assert_equal 1 (ShardByShard.cur_input_index sbs)
      end
    end
  end;
  begin (* str *)
    begin
      let r = ReedSolomon.make 10 3 in
      let sbs = ShardByShard.make r in

      let shards = make_random_shards_str 10_000 13 in

      let (data, parity) = array_split_at shards 10 in

      for i = 0 to (10) - 1 do
        assert_equal i (ShardByShard.cur_input_index sbs);

        ShardByShard.encode_sep_str sbs data parity;
      done;

      assert_bool "parity ready" (ShardByShard.parity_ready sbs);

      assert_equal
        (Error RS_SBS_Error.TooManyCalls)
        (ShardByShard.encode_sep_str_no_exn sbs data parity);

      ShardByShard.reset sbs;

      for i = 0 to (1) - 1 do
        assert_equal i (ShardByShard.cur_input_index sbs);

        ShardByShard.encode_sep_str sbs data parity
      done;

      assert_equal
        (Error RS_SBS_Error.LeftoverShards)
        (ShardByShard.reset_no_exn sbs);

      ShardByShard.reset_force sbs;

      assert_equal 0 (ShardByShard.cur_input_index sbs);
    end;
    begin
      let r = ReedSolomon.make 10 3 in

      begin
        let sbs = ShardByShard.make r in
        let shards = make_random_shards_str 100 13 in
        shards.(0) <- "";

        begin
          let (data, parity) = array_split_at shards 10 in

          assert_equal 0 (ShardByShard.cur_input_index sbs);

          assert_equal
            (Error (RS_SBS_Error.RSError RS_Error.EmptyShard))
            (ShardByShard.encode_sep_str_no_exn sbs data parity);

          assert_equal 0 (ShardByShard.cur_input_index sbs);

          assert_equal
            (Error (RS_SBS_Error.RSError RS_Error.EmptyShard))
            (ShardByShard.encode_sep_str_no_exn sbs data parity);

          assert_equal 0 (ShardByShard.cur_input_index sbs);
        end;

        shards.(0) <- String.make 100 '\000';

        let (data, parity) = array_split_at shards 10 in

        ShardByShard.encode_sep_str sbs data parity;

        assert_equal 1 (ShardByShard.cur_input_index sbs);
      end;
      begin
        let sbs = ShardByShard.make r in

        let shards = make_random_shards_str 100 13 in
        shards.(10) <- "";

        begin
          let (data, parity) = array_split_at shards 10 in

          assert_equal 0 (ShardByShard.cur_input_index sbs);

          assert_equal
            (Error (RS_SBS_Error.RSError RS_Error.EmptyShard))
            (ShardByShard.encode_sep_str_no_exn sbs data parity);

          assert_equal 0 (ShardByShard.cur_input_index sbs);

          assert_equal
            (Error (RS_SBS_Error.RSError RS_Error.EmptyShard))
            (ShardByShard.encode_sep_str_no_exn sbs data parity);

          assert_equal 0 (ShardByShard.cur_input_index sbs);
        end;

        shards.(10) <- String.make 100 '\000';

        let (data, parity) = array_split_at shards 10 in

        ShardByShard.encode_sep_str sbs data parity;

        assert_equal 1 (ShardByShard.cur_input_index sbs);
      end
    end;
    begin
      let r = ReedSolomon.make 10 3 in
      begin
        let sbs = ShardByShard.make r in

        let shards = make_random_shards_str 100 13 in
        shards.(1) <- String.make 99 '\000';
        begin
          let (data, parity) = array_split_at shards 10 in

          assert_equal 0 (ShardByShard.cur_input_index sbs);

          assert_equal
            (Error (RS_SBS_Error.RSError RS_Error.IncorrectShardSize))
            (ShardByShard.encode_sep_str_no_exn sbs data parity);

          assert_equal 0 (ShardByShard.cur_input_index sbs);

          assert_equal
            (Error (RS_SBS_Error.RSError RS_Error.IncorrectShardSize))
            (ShardByShard.encode_sep_str_no_exn sbs data parity);

          assert_equal 0 (ShardByShard.cur_input_index sbs);
        end;

        shards.(1) <- String.make 100 '\000';

        let (data, parity) = array_split_at shards 10 in

        ShardByShard.encode_sep_str sbs data parity;

        assert_equal 1 (ShardByShard.cur_input_index sbs)
      end;
      begin
        let sbs = ShardByShard.make r in

        let shards = make_random_shards_str 100 13 in
        shards.(11) <- String.make 99 '\000';
        begin
          let (data, parity) = array_split_at shards 10 in

          assert_equal 0 (ShardByShard.cur_input_index sbs);

          assert_equal
            (Error (RS_SBS_Error.RSError (RS_Error.IncorrectShardSize)))
            (ShardByShard.encode_sep_str_no_exn sbs data parity);

          assert_equal 0 (ShardByShard.cur_input_index sbs);

          assert_equal
            (Error (RS_SBS_Error.RSError (RS_Error.IncorrectShardSize)))
            (ShardByShard.encode_sep_str_no_exn sbs data parity);

          assert_equal 0 (ShardByShard.cur_input_index sbs);
        end;

        shards.(11) <- String.make 100 '\000';

        let (data, parity) = array_split_at shards 10 in

        ShardByShard.encode_sep_str sbs data parity;

        assert_equal 1 (ShardByShard.cur_input_index sbs)
      end
    end
  end;
  begin (* bigstr *)
    begin
      let r = ReedSolomon.make 10 3 in
      let sbs = ShardByShard.make r in

      let shards = make_random_shards_bigstr 10_000 13 in

      let (data, parity) = array_split_at shards 10 in

      for i = 0 to (10) - 1 do
        assert_equal i (ShardByShard.cur_input_index sbs);

        ShardByShard.encode_sep_bigstr sbs data parity;
      done;

      assert_bool "parity ready" (ShardByShard.parity_ready sbs);

      assert_equal
        (Error RS_SBS_Error.TooManyCalls)
        (ShardByShard.encode_sep_bigstr_no_exn sbs data parity);

      ShardByShard.reset sbs;

      for i = 0 to (1) - 1 do
        assert_equal i (ShardByShard.cur_input_index sbs);

        ShardByShard.encode_sep_bigstr sbs data parity
      done;

      assert_equal
        (Error RS_SBS_Error.LeftoverShards)
        (ShardByShard.reset_no_exn sbs);

      ShardByShard.reset_force sbs;

      assert_equal 0 (ShardByShard.cur_input_index sbs);
    end;
    begin
      let r = ReedSolomon.make 10 3 in

      begin
        let sbs = ShardByShard.make r in
        let shards = make_random_shards_bigstr 100 13 in
        shards.(0) <- Bigstring.create 0;

        begin
          let (data, parity) = array_split_at shards 10 in

          assert_equal 0 (ShardByShard.cur_input_index sbs);

          assert_equal
            (Error (RS_SBS_Error.RSError RS_Error.EmptyShard))
            (ShardByShard.encode_sep_bigstr_no_exn sbs data parity);

          assert_equal 0 (ShardByShard.cur_input_index sbs);

          assert_equal
            (Error (RS_SBS_Error.RSError RS_Error.EmptyShard))
            (ShardByShard.encode_sep_bigstr_no_exn sbs data parity);

          assert_equal 0 (ShardByShard.cur_input_index sbs);
        end;

        shards.(0) <- Bigstring.create 100;

        let (data, parity) = array_split_at shards 10 in

        ShardByShard.encode_sep_bigstr sbs data parity;

        assert_equal 1 (ShardByShard.cur_input_index sbs);
      end;
      begin
        let sbs = ShardByShard.make r in

        let shards = make_random_shards_bigstr 100 13 in
        shards.(10) <- Bigstring.create 0;

        begin
          let (data, parity) = array_split_at shards 10 in

          assert_equal 0 (ShardByShard.cur_input_index sbs);

          assert_equal
            (Error (RS_SBS_Error.RSError RS_Error.EmptyShard))
            (ShardByShard.encode_sep_bigstr_no_exn sbs data parity);

          assert_equal 0 (ShardByShard.cur_input_index sbs);

          assert_equal
            (Error (RS_SBS_Error.RSError RS_Error.EmptyShard))
            (ShardByShard.encode_sep_bigstr_no_exn sbs data parity);

          assert_equal 0 (ShardByShard.cur_input_index sbs);
        end;

        shards.(10) <- Bigstring.create 100;

        let (data, parity) = array_split_at shards 10 in

        ShardByShard.encode_sep_bigstr sbs data parity;

        assert_equal 1 (ShardByShard.cur_input_index sbs);
      end
    end;
    begin
      let r = ReedSolomon.make 10 3 in
      begin
        let sbs = ShardByShard.make r in

        let shards = make_random_shards_bigstr 100 13 in
        shards.(1) <- Bigstring.create 99;
        begin
          let (data, parity) = array_split_at shards 10 in

          assert_equal 0 (ShardByShard.cur_input_index sbs);

          assert_equal
            (Error (RS_SBS_Error.RSError RS_Error.IncorrectShardSize))
            (ShardByShard.encode_sep_bigstr_no_exn sbs data parity);

          assert_equal 0 (ShardByShard.cur_input_index sbs);

          assert_equal
            (Error (RS_SBS_Error.RSError RS_Error.IncorrectShardSize))
            (ShardByShard.encode_sep_bigstr_no_exn sbs data parity);

          assert_equal 0 (ShardByShard.cur_input_index sbs);
        end;

        shards.(1) <- Bigstring.create 100;

        let (data, parity) = array_split_at shards 10 in

        ShardByShard.encode_sep_bigstr sbs data parity;

        assert_equal 1 (ShardByShard.cur_input_index sbs)
      end;
      begin
        let sbs = ShardByShard.make r in

        let shards = make_random_shards_bigstr 100 13 in
        shards.(11) <- Bigstring.create 99;
        begin
          let (data, parity) = array_split_at shards 10 in

          assert_equal 0 (ShardByShard.cur_input_index sbs);

          assert_equal
            (Error (RS_SBS_Error.RSError (RS_Error.IncorrectShardSize)))
            (ShardByShard.encode_sep_bigstr_no_exn sbs data parity);

          assert_equal 0 (ShardByShard.cur_input_index sbs);

          assert_equal
            (Error (RS_SBS_Error.RSError (RS_Error.IncorrectShardSize)))
            (ShardByShard.encode_sep_bigstr_no_exn sbs data parity);

          assert_equal 0 (ShardByShard.cur_input_index sbs);
        end;

        shards.(11) <- Bigstring.create 100;

        let (data, parity) = array_split_at shards 10 in

        ShardByShard.encode_sep_bigstr sbs data parity;

        assert_equal 1 (ShardByShard.cur_input_index sbs)
      end
    end
  end

let test_encode_single_sep test_ctxt =
  let r = ReedSolomon.make 10 3 in

  begin (* bytes *)
    let shards = make_random_shards_bytes 10 13 in

    ReedSolomon.encode_bytes r shards;

    let shards_copy = copy_shards_bytes shards in

    begin
      let (data, parity) = array_split_at shards_copy 10 in

      for i = 0 to (10) - 1 do
        ReedSolomon.encode_single_sep_bytes r i data.(i) parity;
      done
    end;
    assert_bool "verification" (ReedSolomon.verify_bytes r shards);
    assert_bool "verification" (ReedSolomon.verify_bytes r shards_copy);

    assert_equal shards shards_copy;
  end;
  begin (* str *)
    let shards = make_random_shards_str 10 13 in

    ReedSolomon.encode_str r shards;

    let shards_copy = copy_shards_str shards in

    begin
      let (data, parity) = array_split_at shards_copy 10 in

      for i = 0 to (10) - 1 do
        ReedSolomon.encode_single_sep_str r i data.(i) parity;
      done
    end;
    assert_bool "verification" (ReedSolomon.verify_str r shards);
    assert_bool "verification" (ReedSolomon.verify_str r shards_copy);

    assert_equal shards shards_copy;
  end;
  begin (* bigstr *)
    let shards = make_random_shards_bigstr 10 13 in

    ReedSolomon.encode_bigstr r shards;

    let shards_copy = copy_shards_bigstr shards in

    begin
      let (data, parity) = array_split_at shards_copy 10 in

      for i = 0 to (10) - 1 do
        ReedSolomon.encode_single_sep_bigstr r i data.(i) parity;
      done
    end;
    assert_bool "verification" (ReedSolomon.verify_bigstr r shards);
    assert_bool "verification" (ReedSolomon.verify_bigstr r shards_copy);

    assert_equal shards shards_copy;
  end

let test_encode_sep test_ctxt =
  let r = ReedSolomon.make 10 3 in

  begin (* bytes *)
    let shards = make_random_shards_bytes 10_000 13 in

    ReedSolomon.encode_bytes r shards;

    let shards_copy = copy_shards_bytes shards in

    begin
      let (data, parity) = array_split_at shards_copy 10 in

      ReedSolomon.encode_sep_bytes r data parity
    end;

    assert_equal shards shards_copy
  end;
  begin (* str *)
    let shards = make_random_shards_str 10_000 13 in

    ReedSolomon.encode_str r shards;

    let shards_copy = copy_shards_str shards in

    begin
      let (data, parity) = array_split_at shards_copy 10 in

      ReedSolomon.encode_sep_str r data parity
    end;

    assert_equal shards shards_copy
  end;
  begin (* bigstr *)
    let shards = make_random_shards_bigstr 10_000 13 in

    ReedSolomon.encode_bigstr r shards;

    let shards_copy = copy_shards_bigstr shards in

    begin
      let (data, parity) = array_split_at shards_copy 10 in

      ReedSolomon.encode_sep_bigstr r data parity
    end;

    assert_equal shards shards_copy
  end

let test_encode_single_sep_error_handling test_ctxt =
  let r = ReedSolomon.make 10 3 in

  begin (* bytes *)
    let shards = make_random_shards_bytes 1000 13 in

    begin
      let (data, parity) = array_split_at shards 10 in

      for i = 0 to (10) - 1 do
        ReedSolomon.encode_single_sep_bytes r i data.(i) parity;
      done;

      assert_equal
        (Error RS_Error.InvalidIndex)
        (ReedSolomon.encode_single_sep_bytes_no_exn r 10 data.(0) parity);
      assert_equal
        (Error RS_Error.InvalidIndex)
        (ReedSolomon.encode_single_sep_bytes_no_exn r 11 data.(0) parity);
      assert_equal
        (Error RS_Error.InvalidIndex)
        (ReedSolomon.encode_single_sep_bytes_no_exn r 12 data.(0) parity);
      assert_equal
        (Error RS_Error.InvalidIndex)
        (ReedSolomon.encode_single_sep_bytes_no_exn r 13 data.(0) parity);
      assert_equal
        (Error RS_Error.InvalidIndex)
        (ReedSolomon.encode_single_sep_bytes_no_exn r 14 data.(0) parity);
    end;

    begin
      let (data, parity) = array_split_at shards 11 in

      assert_equal
        (Error RS_Error.TooFewParityShards)
        (ReedSolomon.encode_single_sep_bytes_no_exn r 0 data.(0) parity)
    end;
    begin
      let (data, parity) = array_split_at shards 9 in

      assert_equal
        (Error RS_Error.TooManyParityShards)
        (ReedSolomon.encode_single_sep_bytes_no_exn r 0 data.(0) parity)
    end
  end;
  begin (* str *)
    let shards = make_random_shards_str 1000 13 in

    begin
      let (data, parity) = array_split_at shards 10 in

      for i = 0 to (10) - 1 do
        ReedSolomon.encode_single_sep_str r i data.(i) parity;
      done;

      assert_equal
        (Error RS_Error.InvalidIndex)
        (ReedSolomon.encode_single_sep_str_no_exn r 10 data.(0) parity);
      assert_equal
        (Error RS_Error.InvalidIndex)
        (ReedSolomon.encode_single_sep_str_no_exn r 11 data.(0) parity);
      assert_equal
        (Error RS_Error.InvalidIndex)
        (ReedSolomon.encode_single_sep_str_no_exn r 12 data.(0) parity);
      assert_equal
        (Error RS_Error.InvalidIndex)
        (ReedSolomon.encode_single_sep_str_no_exn r 13 data.(0) parity);
      assert_equal
        (Error RS_Error.InvalidIndex)
        (ReedSolomon.encode_single_sep_str_no_exn r 14 data.(0) parity);
    end;

    begin
      let (data, parity) = array_split_at shards 11 in

      assert_equal
        (Error RS_Error.TooFewParityShards)
        (ReedSolomon.encode_single_sep_str_no_exn r 0 data.(0) parity)
    end;
    begin
      let (data, parity) = array_split_at shards 9 in

      assert_equal
        (Error RS_Error.TooManyParityShards)
        (ReedSolomon.encode_single_sep_str_no_exn r 0 data.(0) parity)
    end
  end;
  begin (* bigstr *)
    let shards = make_random_shards_bigstr 1000 13 in

    begin
      let (data, parity) = array_split_at shards 10 in

      for i = 0 to (10) - 1 do
        ReedSolomon.encode_single_sep_bigstr r i data.(i) parity;
      done;

      assert_equal
        (Error RS_Error.InvalidIndex)
        (ReedSolomon.encode_single_sep_bigstr_no_exn r 10 data.(0) parity);
      assert_equal
        (Error RS_Error.InvalidIndex)
        (ReedSolomon.encode_single_sep_bigstr_no_exn r 11 data.(0) parity);
      assert_equal
        (Error RS_Error.InvalidIndex)
        (ReedSolomon.encode_single_sep_bigstr_no_exn r 12 data.(0) parity);
      assert_equal
        (Error RS_Error.InvalidIndex)
        (ReedSolomon.encode_single_sep_bigstr_no_exn r 13 data.(0) parity);
      assert_equal
        (Error RS_Error.InvalidIndex)
        (ReedSolomon.encode_single_sep_bigstr_no_exn r 14 data.(0) parity);
    end;

    begin
      let (data, parity) = array_split_at shards 11 in

      assert_equal
        (Error RS_Error.TooFewParityShards)
        (ReedSolomon.encode_single_sep_bigstr_no_exn r 0 data.(0) parity)
    end;
    begin
      let (data, parity) = array_split_at shards 9 in

      assert_equal
        (Error RS_Error.TooManyParityShards)
        (ReedSolomon.encode_single_sep_bigstr_no_exn r 0 data.(0) parity)
    end
  end

let test_encode_sep_error_handling test_ctxt =
  let r = ReedSolomon.make 10 3 in

  begin (* bytes *)
    begin
      let shards = make_random_shards_bytes 1000 13 in

      let (data, parity) = array_split_at shards 10 in

      ReedSolomon.encode_sep_bytes r data parity;

      begin
        let shards = make_random_shards_bytes 1000 12 in
        let (data, parity) = array_split_at shards 9 in

        assert_equal
          (Error RS_Error.TooFewDataShards)
          (ReedSolomon.encode_sep_bytes_no_exn r data parity)
      end;
      begin
        let shards = make_random_shards_bytes 1000 14 in
        let (data, parity) = array_split_at shards 11 in

        assert_equal
          (Error RS_Error.TooManyDataShards)
          (ReedSolomon.encode_sep_bytes_no_exn r data parity)
      end;
      begin
        let shards = make_random_shards_bytes 1000 12 in
        let (data, parity) = array_split_at shards 10 in

        assert_equal
          (Error RS_Error.TooFewParityShards)
          (ReedSolomon.encode_sep_bytes_no_exn r data parity)
      end;
      begin
        let shards = make_random_shards_bytes 1000 14 in
        let (data, parity) = array_split_at shards 10 in

        assert_equal
          (Error RS_Error.TooManyParityShards)
          (ReedSolomon.encode_sep_bytes_no_exn r data parity)
      end
    end
  end;
  begin (* str *)
    begin
      let shards = make_random_shards_str 1000 13 in

      let (data, parity) = array_split_at shards 10 in

      ReedSolomon.encode_sep_str r data parity;

      begin
        let shards = make_random_shards_str 1000 12 in
        let (data, parity) = array_split_at shards 9 in

        assert_equal
          (Error RS_Error.TooFewDataShards)
          (ReedSolomon.encode_sep_str_no_exn r data parity)
      end;
      begin
        let shards = make_random_shards_str 1000 14 in
        let (data, parity) = array_split_at shards 11 in

        assert_equal
          (Error RS_Error.TooManyDataShards)
          (ReedSolomon.encode_sep_str_no_exn r data parity)
      end;
      begin
        let shards = make_random_shards_str 1000 12 in
        let (data, parity) = array_split_at shards 10 in

        assert_equal
          (Error RS_Error.TooFewParityShards)
          (ReedSolomon.encode_sep_str_no_exn r data parity)
      end;
      begin
        let shards = make_random_shards_str 1000 14 in
        let (data, parity) = array_split_at shards 10 in

        assert_equal
          (Error RS_Error.TooManyParityShards)
          (ReedSolomon.encode_sep_str_no_exn r data parity)
      end
    end
  end;
  begin (* bigstr *)
    begin
      let shards = make_random_shards_bigstr 1000 13 in

      let (data, parity) = array_split_at shards 10 in

      ReedSolomon.encode_sep_bigstr r data parity;

      begin
        let shards = make_random_shards_bigstr 1000 12 in
        let (data, parity) = array_split_at shards 9 in

        assert_equal
          (Error RS_Error.TooFewDataShards)
          (ReedSolomon.encode_sep_bigstr_no_exn r data parity)
      end;
      begin
        let shards = make_random_shards_bigstr 1000 14 in
        let (data, parity) = array_split_at shards 11 in

        assert_equal
          (Error RS_Error.TooManyDataShards)
          (ReedSolomon.encode_sep_bigstr_no_exn r data parity)
      end;
      begin
        let shards = make_random_shards_bigstr 1000 12 in
        let (data, parity) = array_split_at shards 10 in

        assert_equal
          (Error RS_Error.TooFewParityShards)
          (ReedSolomon.encode_sep_bigstr_no_exn r data parity)
      end;
      begin
        let shards = make_random_shards_bigstr 1000 14 in
        let (data, parity) = array_split_at shards 10 in

        assert_equal
          (Error RS_Error.TooManyParityShards)
          (ReedSolomon.encode_sep_bigstr_no_exn r data parity)
      end
    end
  end

let test_encode_single_error_handling test_ctxt =
  let r = ReedSolomon.make 10 3 in

  begin (* bytes *)
    let shards = make_random_shards_bytes 1000 13 in

    for i = 0 to (10) - 1 do
      ReedSolomon.encode_single_bytes r i shards
    done;

    assert_equal
      (Error RS_Error.InvalidIndex)
      (ReedSolomon.encode_single_bytes_no_exn r 10 shards);
    assert_equal
      (Error RS_Error.InvalidIndex)
      (ReedSolomon.encode_single_bytes_no_exn r 11 shards);
    assert_equal
      (Error RS_Error.InvalidIndex)
      (ReedSolomon.encode_single_bytes_no_exn r 12 shards);
    assert_equal
      (Error RS_Error.InvalidIndex)
      (ReedSolomon.encode_single_bytes_no_exn r 13 shards);
    assert_equal
      (Error RS_Error.InvalidIndex)
      (ReedSolomon.encode_single_bytes_no_exn r 14 shards);
  end;
  begin (* str *)
    let shards = make_random_shards_str 1000 13 in

    for i = 0 to (10) - 1 do
      ReedSolomon.encode_single_str r i shards
    done;

    assert_equal
      (Error RS_Error.InvalidIndex)
      (ReedSolomon.encode_single_str_no_exn r 10 shards);
    assert_equal
      (Error RS_Error.InvalidIndex)
      (ReedSolomon.encode_single_str_no_exn r 11 shards);
    assert_equal
      (Error RS_Error.InvalidIndex)
      (ReedSolomon.encode_single_str_no_exn r 12 shards);
    assert_equal
      (Error RS_Error.InvalidIndex)
      (ReedSolomon.encode_single_str_no_exn r 13 shards);
    assert_equal
      (Error RS_Error.InvalidIndex)
      (ReedSolomon.encode_single_str_no_exn r 14 shards);
  end;
  begin (* bigstr *)
    let shards = make_random_shards_bigstr 1000 13 in

    for i = 0 to (10) - 1 do
      ReedSolomon.encode_single_bigstr r i shards
    done;

    assert_equal
      (Error RS_Error.InvalidIndex)
      (ReedSolomon.encode_single_bigstr_no_exn r 10 shards);
    assert_equal
      (Error RS_Error.InvalidIndex)
      (ReedSolomon.encode_single_bigstr_no_exn r 11 shards);
    assert_equal
      (Error RS_Error.InvalidIndex)
      (ReedSolomon.encode_single_bigstr_no_exn r 12 shards);
    assert_equal
      (Error RS_Error.InvalidIndex)
      (ReedSolomon.encode_single_bigstr_no_exn r 13 shards);
    assert_equal
      (Error RS_Error.InvalidIndex)
      (ReedSolomon.encode_single_bigstr_no_exn r 14 shards);
  end

let suite =
  "reed_solomon_erasure_tests">:::
  ["test_no_data_shards">::                               test_no_data_shards;
   "test_negative_data_shards">::                         test_negative_data_shards;
   "test_no_parity_shards">::                             test_no_parity_shards;
   "test_negative_parity_shards">::                       test_negative_parity_shards;
   "test_too_many_shards">::                              test_too_many_shards;
   "test_shard_count">::                                  test_shard_count;
   "test_shards_to_option_shards_to_shards">::            test_shards_to_option_shards_to_shards;
   "test_option_shards_to_shards_missing_shards_case1">:: test_option_shards_to_shards_missing_shards_case1;
   "test_option_shards_to_shards_missing_shards_case2">:: test_option_shards_to_shards_missing_shards_case2;
   "test_option_shards_to_missing_shards">::              test_option_shards_to_missing_shards;
   "test_encoding">::                                     test_encoding;
   "test_reconstruct_opt">::                              test_reconstruct_opt;
   "test_reconstruct">::                                  test_reconstruct;
   qc_encode_verify_reconstruct_verify;
   qc_encode_verify_reconstruct_verify_opt;
   qc_verify;
   qc_encode_sep_same_as_encode;
   qc_encode_single_same_as_encode;
   qc_encode_single_sep_same_as_encode;
   "test_reconstruct_error_handling">::                     test_reconstruct_error_handling;
   "test_one_encode">::                                     test_one_encode;
   "test_verify_too_few_shards">::                          test_verify_too_few_shards;
   "test_verify_with_buffer_incorrect_buffer_sizes">::      test_verify_with_buffer_incorrect_buffer_sizes;
   "test_verify_with_buffer_gives_correct_parity_shards">:: test_verify_with_buffer_gives_correct_parity_shards;
   "test_slices_or_shards_count_check">::                   test_slices_or_shards_count_check;
   "test_check_slices_or_shards_size">::                    test_check_slices_or_shards_size;
   "shardbyshard_encode_correctly">::                       shardbyshard_encode_correctly;
   qc_shardbyshard_encode_same_as_encode;
   "shardbyshard_encode_sep_correctly">::                   shardbyshard_encode_sep_correctly;
   qc_shardbyshard_encode_sep_same_as_encode;
   "shardbyshard_encode_correctly_more_rigorous">::         shardbyshard_encode_correctly_more_rigorous;
   "shardbyshard_encode_error_handling">::                  shardbyshard_encode_error_handling;
   "shardbyshard_encode_sep_error_handling">::              shardbyshard_encode_sep_error_handling;
   "test_encode_single_sep">::                              test_encode_single_sep;
   "test_encode_sep">::                                     test_encode_sep;
   "test_encode_single_sep_error_handling">::               test_encode_single_sep_error_handling;
   "test_encode_sep_error_handling">::                      test_encode_sep_error_handling;
   "test_encode_single_error_handling">::                   test_encode_single_error_handling;
  ]
