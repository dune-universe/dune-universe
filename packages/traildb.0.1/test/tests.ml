open OUnit2
open Util

let temp_traildb_file () =
  let path = Filename.temp_file "traildb_" "_test" in
  path ^ ".tdb"

let check_empty_db test_ctxt =
    let file = temp_traildb_file () in
    create_traildb ~file ~fields:["field1";"field2"] ~events:[] ;
    assert_equal 3L (traildb_num_fields file);
    assert_equal 0L (traildb_num_trails file);
    assert_equal 0L (traildb_num_events file)

let check_single_trail test_ctxt =
    let file = temp_traildb_file () in
    let trail = "UTMB" in
    let fields = ["field1";"field2"] in
    let trail_events = [
      [ "12345" ; "1_1"; "1_2" ];
      [ "12346" ; "2_1"; "2_2" ];
      [ "12347" ; "3_1"; "3_2" ];
      [ "12348" ; "4_1"; "4_2" ];
      [ "12349" ; "5_1"; "5_2" ];
    ] in
    let events = List.map (fun event -> trail::event) trail_events in
    create_traildb ~file ~fields ~events;
    assert_equal 3L (traildb_num_fields file);
    assert_equal 1L (traildb_num_trails file);
    assert_equal 5L (traildb_num_events file);
    assert_equal (Some trail_events) (traildb_trail file trail)

let creating_a_known_file test_ctxt =
  assert_raises TrailDB.(Error TDB_ERR_IO_OPEN)
  (fun () -> create_traildb (Filename.temp_file "traildb_" "_test") ["a"] [])

let opening_an_unknown_file test_ctxt =
  assert_raises TrailDB.(Error TDB_ERR_IO_OPEN)
  (fun () -> traildb_num_trails "/foo")

let suite = "tests">:::[
  "check_empty_db" >:: check_empty_db;
  "check_single_trail" >:: check_single_trail;
  "creating_a_known_file" >:: creating_a_known_file;
  "opening_an_unknown_file" >:: opening_an_unknown_file;
]

let () =                                                                                                                                                                                                           
  run_test_tt_main suite
