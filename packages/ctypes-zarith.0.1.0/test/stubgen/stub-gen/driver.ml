let _ =
  let prefix = "test_stubs" in
  let generate_ml, generate_c = (ref false, ref false) in
  Arg.parse
    [
      ("-ml", Arg.Set generate_ml, "Generate ML");
      ("-c", Arg.Set generate_c, "Generate C");
    ]
    (fun _ -> failwith "unexpected anonymous argument")
    "stubgen [-ml|-c]";
  match (!generate_ml, !generate_c) with
  | false, false | true, true ->
    failwith "Exactly one of -ml and -c must be specified"
  | true, false ->
    Cstubs.write_ml Format.std_formatter ~prefix
      (module Ctypes_zarith_test_stubs.Stubs)
  | false, true ->
    print_endline "#include <gmp.h>";
    Cstubs.write_c Format.std_formatter ~prefix
      (module Ctypes_zarith_test_stubs.Stubs)
