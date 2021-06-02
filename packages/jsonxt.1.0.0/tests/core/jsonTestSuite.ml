let string_parse_std jsons =
  match Jsonxt.Strict.json_of_string jsons with
  | Ok _ -> `Pass
  | Error _ -> `Fail

let string_parse_stream jsons =
  let stream = Jsonxt.Strict_stream.json_stream_of_string jsons in
  let rec loop () =
    match Jsonxt.Strict_stream.decode_stream stream with
    | Error _ -> `Fail
    | Ok None -> `Pass
    | Ok Some _ -> loop ()
  in
  loop ()

let string_parse_monad jsons =
  let open Utils.IO in
  let iobuf = Utils.StringIO.create jsons in
  let reader buf len = Utils.StringIO.read iobuf buf len |> Utils.IO.return in
  let module JsonIO = Jsonxt.Strict_monad.Make(Utils.IO) in
  match result (JsonIO.read_json ~reader ()) with
  | Ok _ -> `Pass
  | Error _ -> `Fail

let filename_to_success filename =
  let name = Filename.basename filename in
  match String.get name 0 with
  | 'y' | 'Y' -> `Pass
  | 'n' | 'N' -> `Fail
  | 'i' | 'I' -> `Undefined
  | _         -> `Undefined

let result_to_string = function
  | `Pass -> "pass"
  | `Fail -> "fail"
  | `Undefined -> "undef"

let pass_fail =
  let pp ppf v = Fmt.pf ppf "%s" (result_to_string v) in
  let pass_fail_eq expected result =
    match expected, result with
    | `Pass, `Pass -> true
    | `Fail, `Fail -> true
    | `Undefined, _ -> true
    | _ -> false
  in
  Alcotest.testable pp pass_fail_eq

let test_parse_file filename parser_f () =
  let jsons = Utils.load_file filename in
  let expected = filename_to_success filename in
  let result = parser_f jsons in
  Alcotest.(check pass_fail) filename expected result 

let gen_tests parser_name parser_f files =
  let create_test file =
    let msg = Filename.basename file in
    Alcotest.test_case msg `Quick (test_parse_file file parser_f)
  in
  let tests = List.map create_test files in
  [ parser_name, tests ]

let test_suite_std files =
  let alco_opts = [] in
  let argv = Array.of_list ("suite"::alco_opts) in
  let alco_tests = gen_tests "standard" string_parse_std files in
  Alcotest.run ~argv "Suite" alco_tests

let test_suite_stream files =
  let alco_opts = [] in
  let argv = Array.of_list ("suite"::alco_opts) in
  let alco_tests = gen_tests "stream" string_parse_stream files in
  Alcotest.run ~argv "Suite" alco_tests

let test_suite_monad files =
  let alco_opts = [] in
  let argv = Array.of_list ("suite"::alco_opts) in
  let alco_tests = gen_tests "monad" string_parse_monad files in
  Alcotest.run ~argv "Suite" alco_tests

let test_suite_all files =
  let alco_opts = [] in
  let argv = Array.of_list ("suite"::alco_opts) in
  let alco_tests =
    gen_tests "standard" string_parse_std files @
    gen_tests "stream" string_parse_monad files @
    gen_tests "monad" string_parse_monad files
  in
  Alcotest.run ~argv "Suite" alco_tests
