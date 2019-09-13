open OUnit2

let program = Conf.make_exec "main"

let () = run_test_tt_main Test_amf.test
