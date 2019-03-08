open OUnit

module Q = Queue

type func = Write of (bytes * int * int) | Close

module Mock_sender =
  struct
    type t = { func_list:func Q.t; result_list:int option Q.t }

    let create funcs results = { func_list=funcs; result_list=results }

    let write sender buf start len =
        Q.push (Write (buf, start, len)) sender.func_list;
        Q.pop sender.result_list

    let close sender = Q.push Close sender.func_list
  end

module Mock_logger = Fluent_logger.Make(Mock_sender)
module FL = Mock_logger

type params = {
  tag:string; time:Int64.t; record:Msgpack.Serialize.t; packed:bytes
}

let times n f init =
  let rec _times i a =
    if i >= n then a
    else _times (i + 1) (f i a) in
  _times 0 init

let mp_str s = `FixRaw (ExtString.String.explode s)

let gen_packed_buf ?(time=Int64.of_float (Unix.time ())) tag record =
  let packed = Msgpack.Serialize.serialize_string
                 (`FixArray [mp_str tag; `Uint32 time; record]) in
  (time, Bytes.of_string packed)

let gen_params_from_idx i =
  let tags = ["development"; "staging"; "production"; "dr"] in
  let tags_len = List.length tags in
  let tag = List.nth tags (i mod tags_len) in
  let record = `FixMap [
    (mp_str "name", mp_str ("foobar" ^ (string_of_int (i mod 17))));
    (mp_str "age", `Uint8 (i mod 100));
    (mp_str "pi", `Float 3.14)
  ] in
  let time = Int64.of_int (1000000000 + i) in
  let (_, packed) = gen_packed_buf ~time:time tag record in
  {tag=tag; time=time; record=record; packed=packed}

let test_logger_post () =
  (* setup Mock_logger *)
  let params = gen_params_from_idx 0 in
  let results = Q.create () in
  Q.push (Some (Bytes.length params.packed)) results;
  Q.push None results;
  let funcs = Q.create () in
  let sender = Mock_sender.create funcs results in
  let logger = FL.create_with_sender sender in
  (* first sending (result:ok) *)
  assert_equal true
    (FL.post_with_time logger params.tag params.record params.time);
  assert_equal
    (Write (params.packed, 0, (Bytes.length params.packed)))
    (Q.pop funcs);
  (* releasing logger *)
  FL.release logger;
  assert_equal Close (Q.pop funcs);
  assert_equal 0 (Q.length funcs)

let test_logger_post_sending_partial () =
  (* setup Mock_logger *)
  let tag = "development" in
  let value = Bytes.create 10000 in
  Bytes.iteri (fun i _ -> Bytes.set value i (Char.chr (0x30 + (i mod 7)))) value;
  let record = mp_str (Bytes.to_string value) in
  let (time, packed) = gen_packed_buf tag record in
  let write_size = 1500 in
  let packed_size = Bytes.length packed in
  let write_count = packed_size / write_size in
  let last_write_size = packed_size mod write_size in
  let results = Q.create () in
  times write_count (fun _ _ -> Q.push (Some write_size) results) ();
  Q.push (Some last_write_size) results;
  Q.push None results;
  let funcs = Q.create () in
  let sender = Mock_sender.create funcs results in
  let logger = FL.create_with_sender sender in
  (* first sending (result:ok) *)
  assert_equal true (FL.post_with_time logger tag record time);
  times (write_count + 1) (fun i _ ->
    assert_equal
      (Write (packed, i * write_size, packed_size - i * write_size))
      (Q.pop funcs)
  ) ();
  (* releasing logger *)
  FL.release logger;
  assert_equal Close (Q.pop funcs);
  assert_equal 0 (Q.length funcs)

let test_logger_post_retry () =
  (* setup Mock_logger *)
  let loop_max = 3 in
  let params_list = times loop_max (fun i a -> (gen_params_from_idx i)::a) [] in
  let results = Q.create () in
  Q.push (Some (Bytes.length (List.nth params_list 0).packed)) results;
  Q.push None results;
  Q.push (Some (
    (Bytes.length (List.nth params_list 1).packed) +
    (Bytes.length (List.nth params_list 2).packed)
  )) results;
  Q.push None results;
  let funcs = Q.create () in
  let sender = Mock_sender.create funcs results in
  let logger = FL.create_with_sender sender in
  (* first sending (result:ok) *)
  let params = List.nth params_list 0 in
  assert_equal
    true (FL.post_with_time logger params.tag params.record params.time);
  assert_equal
    (Write (params.packed, 0, (Bytes.length params.packed)))
    (Q.pop funcs);
  (* second sending (result:ng) *)
  let params = List.nth params_list 1 in
  assert_equal
    false (FL.post_with_time logger params.tag params.record params.time);
  assert_equal
    (Write (params.packed, 0, (Bytes.length params.packed)))
    (Q.pop funcs);
  assert_equal Close (Q.pop funcs);
  (* third sending (result:ok) *)
  let params = List.nth params_list 2 in
  assert_equal
    true (FL.post_with_time logger params.tag params.record params.time);
  let joined_packed = 
    Bytes.cat (List.nth params_list 1).packed (List.nth params_list 2).packed in
  assert_equal
    (Write (joined_packed, 0, Bytes.length joined_packed))
    (Q.pop funcs);
  (* releasing logger *)
  FL.release logger;
  assert_equal Close (Q.pop funcs);
  assert_equal 0 (Q.length funcs)

let test_logger_post_many_times () =
  (* setup Mock_logger *)
  let loop_max = 10000 in
  let params_list = times loop_max (fun i a -> (gen_params_from_idx i)::a) [] in
  let results = Q.create () in
  List.iter (fun params ->
    Q.push (Some (Bytes.length params.packed)) results;
  ) params_list;
  Q.push None results;
  let funcs = Q.create () in
  let sender = Mock_sender.create funcs results in
  let logger = FL.create_with_sender sender in
  ignore (
    times loop_max (fun i _ ->
      let params = List.nth params_list i in
      (* sending (result:ok) *)
      assert_equal
        true (FL.post_with_time logger params.tag params.record params.time);
      assert_equal
        (Write (params.packed, 0, (Bytes.length params.packed)))
        (Q.pop funcs);
    ) ()
  );
  (* releasing logger *)
  FL.release logger;
  assert_equal Close (Q.pop funcs);
  assert_equal 0 (Q.length funcs)

let suite =
  "Fluent_logger tests" >:::
    ["test_logger_post"                 >:: test_logger_post;
     "test_logger_post_sending_partial" >:: test_logger_post_sending_partial;
     "test_logger_post_retry"           >:: test_logger_post_retry;
     "test_logger_post_many_times"      >:: test_logger_post_many_times;
    ]

let () =
  Printexc.record_backtrace true;
  ignore (run_test_tt_main suite)

