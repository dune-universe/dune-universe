module JsonSexp = struct
  open Core_kernel
  type json = [
       `Null | `Bool of bool | `Int of int | `Intlit of string | `Float of float
    | `Floatlit of string | `String of string | `Stringlit of string
    | `Assoc of (string * json) list | `List of json list | `Tuple of json list
    | `Variant of (string * json option) ] [@@deriving sexp]
end

module JsonStreamSexp = struct
  open Core_kernel
  type json_stream = [
      `Null | `Bool of bool | `Int of int | `Intlit of string | `Float of float
    | `Floatlit of string | `String of string | `Stringlit of string
    | `As | `Ae | `Os | `Oe | `Ts | `Te | `Vs | `Ve | `Name of string
    | `Infinity | `Neg_infinity | `Nan ] list [@@deriving sexp]
end

let regexp_nan = Str.regexp_string "__NAN__"
let string_nan = String.uppercase_ascii (string_of_float (0.0/.0.0))

let read_json_sexp inc =
  let l = input_line inc in
  let p = Utils.split_string '\t' l in
  let (bits, jsons, sexps, sexps_json_stream) = match p with
    | bt::jv::sv::ssv::[] -> (bt, jv, sv, ssv)
    | bt::jv::[] -> (bt, jv, "", "")
    | _ -> Utils.die ("invalid test line: " ^ l)
  in
  (* Replace __NAN__ with the architecture specific version *)
  let sexps = Str.global_replace regexp_nan string_nan sexps in
  let sexps_json_stream = Str.global_replace regexp_nan string_nan sexps_json_stream in
  (bits, jsons, sexps, sexps_json_stream)

let output_validation_config bits jsons json json_stream =
  Printf.printf "%s\t%s\t%s\t%s\n"
    bits
    jsons
    (JsonSexp.sexp_of_json json |> Core_kernel.Sexp.to_string)
    (JsonStreamSexp.sexp_of_json_stream json_stream |> Core_kernel.Sexp.to_string)

let get_json_stream jsons =
  let stream = Jsonxt.Extended_stream.json_stream_of_string jsons in
  let rec loop res =
    match Jsonxt.Extended_stream.decode_stream stream with
    | Error err -> Error err
    | Ok None -> Ok (List.rev res)
    | Ok Some tok -> loop (tok::res)
  in
  loop []

let parse_json jsons =
  let json_result = Jsonxt.Extended.json_of_string jsons in
  let json_stream_result = get_json_stream jsons in
  let json = 
    match json_result with
    | Ok json -> json
    | Error err -> Printf.sprintf "failed to parse \"%s\": %s" jsons err |> Utils.die
  in
  let json_stream = 
    match json_stream_result with
    | Ok json_stream -> json_stream
    | Error err -> Printf.sprintf "failed to parse stream \"%s\": %s" jsons err |> Utils.die
  in
  (json, json_stream)

let gen_config filename _alco_opts =
  let inc = try open_in filename with | Sys_error err -> Utils.die err in
  let rec loop () =
    match read_json_sexp inc with
    | bits, jsons, sexps, sexps_json_stream -> begin
        match bits with
        | "64" when Utils.int_bits = 32 ->
          Printf.printf "%s\t%s\t%s\t%s\n" bits jsons sexps sexps_json_stream
        | "32" when Utils.int_bits = 64 ->
          Printf.printf "%s\t%s\t%s\t%s\n" bits jsons sexps sexps_json_stream
        | _ ->
          let json, json_stream = parse_json jsons in
          output_validation_config bits jsons json json_stream
      end;
      loop ()
    | exception End_of_file -> ()
  in loop ()

let get_json_stream jsons =
  let stream = Jsonxt.Extended_stream.json_stream_of_string jsons in
  let rec loop res =
    match Jsonxt.Extended_stream.decode_stream stream with
    | Error err -> Error err
    | Ok None -> Ok (List.rev res)
    | Ok Some tok -> loop (tok::res)
  in
  loop []

let json_stream_to_string stream =
  let buf = Buffer.create 100 in
  let add_char c = Buffer.add_char buf c in
  let add_string s = Buffer.add_string buf s in
  let encoder = Jsonxt.Extended_stream.create_encoder ~add_char ~add_string in
  let encode () =
    List.iter (fun v -> Jsonxt.Extended_stream.encode_stream_exn encoder v) stream;
    Buffer.contents buf
  in
  try Ok (encode ()) with
  | Failure err -> Error err

let sexp =
  let pp ppf v = Fmt.pf ppf "%s" (Core_kernel.Sexp.to_string v) in
  let sexp_eq a b = match Core_kernel.Sexp.compare a b with | 0 -> true | _ -> false in
  Alcotest.testable pp sexp_eq

let string_parse_test jsons sexps () =
  let jsonsexp = 
    match Jsonxt.Extended.json_of_string jsons with
    | Ok json -> JsonSexp.sexp_of_json json
    | Error err -> Core_kernel.Sexp.Atom (Printf.sprintf "Failed to parse '%s': %s" jsons err)
  in
  let sexpv = Core_kernel.Sexp.of_string sexps in
  Alcotest.(check sexp) jsons sexpv jsonsexp 

let stream_t_parse_test jsons sexps () =
  let jsons = String.concat " " [jsons; jsons; jsons] in
  let sexps = String.concat "" ["("; sexps; sexps; sexps; ")"] in
  let stream = Jsonxt.Extended.stream_from_string jsons in
  let rec loop res =
    match Stream.next stream with
    | exception Stream.Failure -> Core_kernel.Sexp.List (List.rev res)
    | v -> loop ((JsonSexp.sexp_of_json v)::res)
  in
  let jsonsexp = loop [] in
  let sexpv = Core_kernel.Sexp.of_string sexps in
  Alcotest.(check sexp) jsons sexpv jsonsexp 

let file_write_test jsons sexps tmpfile () =
  let jsonsexp = 
    match Jsonxt.Extended.json_of_string jsons with
    | Ok json -> begin
      match Jsonxt.Extended.json_to_file tmpfile json with
      | Ok () -> begin
          match Jsonxt.Extended.json_of_file tmpfile with
          | Ok json -> JsonSexp.sexp_of_json json
          | Error err ->
            let str = Utils.load_file tmpfile in
            Core_kernel.Sexp.Atom (Printf.sprintf "Failed to re-parse written json '%s': %s" str err)
        end
      | Error err -> Core_kernel.Sexp.Atom (Printf.sprintf "Failed to write parsed json '%s': %s" jsons err)
      end
    | Error err -> Core_kernel.Sexp.Atom (Printf.sprintf "Failed to parse '%s': %s" jsons err)
  in
  let sexpv = Core_kernel.Sexp.of_string sexps in
  Alcotest.(check sexp) jsons sexpv jsonsexp 

let string_write_test' f jsons sexps () =
  let jsonsexp = 
    match Jsonxt.Extended.json_of_string jsons with
    | Ok json -> begin
      match f json with
      | Ok str -> begin
          match Jsonxt.Extended.json_of_string str with
          | Ok json -> JsonSexp.sexp_of_json json
          | Error err -> Core_kernel.Sexp.Atom (Printf.sprintf "Failed to re-parse written json '%s': %s" str err)
        end
      | Error err -> Core_kernel.Sexp.Atom (Printf.sprintf "Failed to write parsed json '%s': %s" jsons err)
      end
    | Error err -> Core_kernel.Sexp.Atom (Printf.sprintf "Failed to parse '%s': %s" jsons err)
  in
  let sexpv = Core_kernel.Sexp.of_string sexps in
  Alcotest.(check sexp) jsons sexpv jsonsexp 

let string_write_test jsons sexp = string_write_test' Jsonxt.Extended.json_to_string jsons sexp
let string_write_hum_test jsons sexp = string_write_test' Jsonxt.Extended.json_to_string_hum jsons sexp

let stream_write_test jsons sexp_streams () =
  let json_stream_sexp = 
    match get_json_stream jsons with
    | Ok json_stream -> begin
      match json_stream_to_string json_stream with
      | Ok str -> begin
          match Jsonxt.Extended.json_of_string str with
          | Ok json -> JsonSexp.sexp_of_json json
          | Error err -> Core_kernel.Sexp.Atom (Printf.sprintf "Failed to re-parse written json stream '%s': %s" str err)
        end
      | Error err -> Core_kernel.Sexp.Atom (Printf.sprintf "Failed to write parsed json_stream '%s': %s" jsons err)
      end
    | Error err -> Core_kernel.Sexp.Atom (Printf.sprintf "Failed to stream parse '%s': %s" jsons err)
  in
  let sexpv = Core_kernel.Sexp.of_string sexp_streams in
  Alcotest.(check sexp) jsons sexpv json_stream_sexp 

let stream_parse_test jsons sexp_streams () =
  let json_stream_sexp = 
    match get_json_stream jsons with
    | Ok json_stream -> JsonStreamSexp.sexp_of_json_stream json_stream
    | Error err -> Core_kernel.Sexp.Atom (Printf.sprintf "Failed to parse '%s': %s" jsons err)
  in
  let sexp_stream = Core_kernel.Sexp.of_string sexp_streams in
  Alcotest.(check sexp) jsons sexp_stream json_stream_sexp 
  
let monad_read jsons =
  let open Utils.IO in
  let iobuf = Utils.StringIO.create jsons in
  let reader buf len = Utils.StringIO.read iobuf buf len |> Utils.IO.return in
  let module JsonIO = Jsonxt.Extended_monad.Make(Utils.IO) in
    result (JsonIO.read_json ~reader ())

let monad_write json =
  let open Utils.IO in
  let iobuf = Utils.StringIO.create "" in
  let writer s = Utils.StringIO.write iobuf s |> Utils.IO.return in
  let module JsonIO = Jsonxt.Extended_monad.Make(Utils.IO) in
  let () = result (JsonIO.write_json ~writer json) in
  Utils.StringIO.contents iobuf

let monad_parse_test jsons sexps () =
  let jsonsexp = match monad_read jsons with
    | Ok json -> JsonSexp.sexp_of_json json
    | Error err -> Core_kernel.Sexp.Atom (Printf.sprintf "Failed to parse '%s': %s" jsons err)
  in
  let sexpv = Core_kernel.Sexp.of_string sexps in
  Alcotest.(check sexp) jsons sexpv jsonsexp 

let monad_write_test jsons sexps () =
  let jsonsexp = 
    match monad_read jsons with
    | Ok json -> begin
        let str = monad_write json in
        match Jsonxt.Extended.json_of_string str with
        | Ok json -> JsonSexp.sexp_of_json json
        | Error err -> Core_kernel.Sexp.Atom (Printf.sprintf "Failed to re-parse written json '%s': %s" str err)
      end
    | Error err -> Core_kernel.Sexp.Atom (Printf.sprintf "Failed to parse '%s': %s" jsons err)
  in
  let sexpv = Core_kernel.Sexp.of_string sexps in
  Alcotest.(check sexp) jsons sexpv jsonsexp 

let gen_tests filename tmpfile = 
  let filter a l = List.filter (fun (k, _) -> k = a) l |> List.map (fun (_, v) -> v) in
  let inc = try open_in filename with | Sys_error err -> Utils.die err in
  let rec loop tests =
    match read_json_sexp inc with
    | bits, jsons, sexps, sexps_json_stream -> begin
      let msg = jsons in
      match bits with
      | "64" when Utils.int_bits = 32 -> loop tests
      | "32" when Utils.int_bits = 64 -> loop tests
      | _ ->
        let tests = (`Std, (Alcotest.test_case msg `Quick (string_parse_test jsons sexps)))::tests in
        let tests = (`Stream, (Alcotest.test_case msg `Quick (stream_parse_test jsons sexps_json_stream)))::tests in
        let tests = (`Std_write, (Alcotest.test_case msg `Quick (string_write_test jsons sexps)))::tests in
        let tests = (`Std_write_hum, (Alcotest.test_case msg `Quick (string_write_hum_test jsons sexps)))::tests in
        let tests = (`File_write, (Alcotest.test_case msg `Quick (file_write_test jsons sexps tmpfile)))::tests in
        let tests = (`Stream_write, (Alcotest.test_case msg `Quick (stream_write_test jsons sexps)))::tests in
        let tests = (`Monad, (Alcotest.test_case msg `Quick (monad_parse_test jsons sexps)))::tests in
        let tests = (`Monad_write, (Alcotest.test_case msg `Quick (monad_write_test jsons sexps)))::tests in
        let tests = (`Stream_t, (Alcotest.test_case msg `Quick (stream_t_parse_test jsons sexps)))::tests in
        loop tests
      end
    | exception End_of_file -> List.rev tests
  in
  let tests = loop [] in
  [
    "standard", filter `Std tests;
    "standard-write-string", filter `Std_write tests;
    "standard-write-string-hum", filter `Std_write_hum tests;
    "standard-write-file", filter `File_write tests;
    "standard-streamt", filter `Stream_t tests;
    "stream", filter `Stream tests;
    "stream-write", filter ` Stream_write tests;
    "monad", filter `Monad tests;
    "monad-write", filter `Monad_write tests
  ]


let run_tests filename alco_opts =
  let argv = Array.of_list ("compliance"::alco_opts) in
  let tmpfile = Filename.temp_file "jxtester" ".json" in
  Alcotest.run ~and_exit:false ~argv "Validation" (gen_tests filename tmpfile);
  try Sys.remove tmpfile with Sys_error _ -> ()
