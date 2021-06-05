let read_test_data inc =
  let l = input_line inc in
  let p = Utils.split_string ' ' l |> List.filter (fun v -> not (String.equal "" v)) in
  let (level, passfail, filename, bits) = match p with
    | lv::pf::fn::"32"::[] -> (lv, pf, fn, "32")
    | lv::pf::fn::"64"::[] -> (lv, pf, fn, "64")
    | lv::pf::fn::"all"::[] -> (lv, pf, fn, "all")
    | lv::pf::fn::[] -> (lv, pf, fn, "all")
    | _ -> Utils.die ("invalid test line: " ^ l)
  in
  let level = match level with
    | "strict"   -> `Strict
    | "basic"    -> `Basic
    | "extended" -> `Extended
    | "yjsafe"   -> `Yojson_safe
    | "yjbasic"  -> `Yojson_basic
    | _ -> Utils.die ("invalid test line, first column invalid level: " ^ l)
  in
  let passfail = match passfail with
    | "pass" | "fail" as v -> v
    | _ -> Utils.die ("invalid test line, second column must be pass or fail: " ^ l)
  in
  (level, passfail, filename, bits)

let get_json_stream of_string decode jsons =
  let stream = of_string jsons in
  let rec loop res =
    match decode stream with
    | Error err -> Error err
    | Ok None -> Ok (List.rev res)
    | Ok Some tok -> loop (tok::res)
  in
  loop []

let of_error f a = match f a with Ok _ -> "pass" | Error _ -> "fail"

let level_to_string = function
  | `Strict -> "strict"
  | `Basic -> "basic"
  | `Extended -> "extended"
  | `Yojson_safe -> "yjsafe"
  | `Yojson_basic -> "yjbasic"

let string_parse_test level filename _passfail =
  let yojson_basic_json_of_string s =
    try Ok (Jsonxt.Yojson.Basic.from_string s) with
    | Failure err -> Error err
    | Jsonxt.Yojson.Json_error err -> Error err
  in
  let yojson_safe_json_of_string s =
    try Ok (Jsonxt.Yojson.Safe.from_string s) with
    | Failure err -> Error err
    | Jsonxt.Yojson.Json_error err -> Error err
  in
  let txt = try Utils.load_file (filename ^ ".json") with Sys_error err -> Utils.die err in
  let string_parser = match level with
    | `Strict       -> of_error Jsonxt.Strict.json_of_string
    | `Basic        -> of_error Jsonxt.Basic.json_of_string
    | `Extended     -> of_error Jsonxt.Extended.json_of_string
    | `Yojson_basic -> of_error yojson_basic_json_of_string
    | `Yojson_safe  -> of_error yojson_safe_json_of_string
  in
  string_parser txt

let file_parse_test level filename _passfail =
  let yojson_basic_json_of_file filename =
    try Ok (Jsonxt.Yojson.Basic.from_file filename) with
    | Failure err -> Error err
    | Jsonxt.Yojson.Json_error err -> Error err
  in
  let yojson_safe_json_of_file filename =
    try Ok (Jsonxt.Yojson.Safe.from_file filename) with
    | Failure err -> Error err
    | Jsonxt.Yojson.Json_error err -> Error err
  in
  let file_parser = match level with
    | `Strict       -> of_error Jsonxt.Strict.json_of_file
    | `Basic        -> of_error Jsonxt.Basic.json_of_file
    | `Extended     -> of_error Jsonxt.Extended.json_of_file
    | `Yojson_basic -> of_error yojson_basic_json_of_file
    | `Yojson_safe  -> of_error yojson_safe_json_of_file
  in
  file_parser (filename ^ ".json")

let stream_parse_test level filename passfail =
  let txt = try Utils.load_file (filename ^ ".json") with Sys_error err -> Utils.die err in
  let stream_parser = match level with
    | `Strict       ->
      of_error (get_json_stream Jsonxt.Strict_stream.json_stream_of_string Jsonxt.Strict_stream.decode_stream)
    | `Basic        ->
      of_error (get_json_stream Jsonxt.Basic_stream.json_stream_of_string Jsonxt.Basic_stream.decode_stream)
    | `Extended     ->
      of_error (get_json_stream Jsonxt.Extended_stream.json_stream_of_string Jsonxt.Extended_stream.decode_stream)
    | `Yojson_basic -> fun _ -> passfail
    | `Yojson_safe  -> fun _ -> passfail
  in 
  stream_parser txt

let monad_parse_test level filename passfail =
  let open Utils.IO in
  let txt = try Utils.load_file (filename ^ ".json") with Sys_error err -> Utils.die err in
  let iobuf = Utils.StringIO.create txt in
  let reader buf len = Utils.StringIO.read iobuf buf len |> Utils.IO.return in
  let of_error res = match res with Ok _ -> return "pass" | Error _ -> return "fail" in
  let result = match level with
    | `Strict       ->
      let module JsonIO = Jsonxt.Strict_monad.Make(Utils.IO) in
      JsonIO.read_json ~reader () >>= of_error
    | `Basic        ->
      let module JsonIO = Jsonxt.Basic_monad.Make(Utils.IO) in
      JsonIO.read_json ~reader () >>= of_error
    | `Extended     ->
      let module JsonIO = Jsonxt.Extended_monad.Make(Utils.IO) in
      JsonIO.read_json ~reader () >>= of_error
    | `Yojson_basic -> return passfail (* monad isn't supported by Yojson *)
    | `Yojson_safe  -> return passfail
  in
  Utils.IO.result result

let tester f level filename passfail () =
  let slevel = level_to_string level in
  let msg = slevel ^ " " ^ filename in
  Alcotest.(check string) msg passfail (f level filename passfail) 

let gen_tests filename =
  let inc = try open_in filename with | Sys_error err -> Utils.die err in
  let rec loop str file monad stream =
    match read_test_data inc with
    | level, passfail, filename, bits -> begin
      let msg = filename ^ " " ^ (level_to_string level) in
      let stest = Alcotest.test_case msg `Quick (tester string_parse_test level filename passfail) in
      let ftest = Alcotest.test_case msg `Quick (tester file_parse_test level filename passfail) in
      let mtest = Alcotest.test_case msg `Quick (tester monad_parse_test level filename passfail) in
      let ttest = Alcotest.test_case msg `Quick (tester stream_parse_test level filename passfail) in
      match bits with
      | "64" when Utils.int_bits = 32 -> loop str file monad stream
      | "32" when Utils.int_bits = 64 -> loop str file monad stream
      | _ -> loop (stest::str) (ftest::file) (mtest::monad) (ttest::stream)
      end
    | exception End_of_file -> (str, file, monad, stream)
  in
  let str_t, file_t, monad_t, stream_t = loop [] [] [] [] in
  [
    "string", (List.rev str_t);
    "file", (List.rev file_t);
    "monad", (List.rev monad_t);
    "stream", (List.rev stream_t) ]

let run_tests filename alco_opts =
  let argv = Array.of_list ("compliance"::alco_opts) in
  Alcotest.run ~argv "Compliance" (gen_tests filename)
