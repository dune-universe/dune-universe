let prefix = "mmdb_stub"

let prologue =
  "\n\
   #if defined(__MINGW32__) || defined(__MINGW64__)\n\
   #define __USE_MINGW_ANSI_STDIO 1\n\
   #include <stdio.h> /* see: https://sourceforge.net/p/mingw-w64/bugs/627/ */\n\
   #endif\n\
   #include <maxminddb.h>\n\
   #include \"mmdb_helpers.h\"\n"

let () =
  let generate_ml, generate_c = ref false, ref false in
  let () =
    Arg.(
      parse
        ["-ml", Set generate_ml, "Generate ML"; "-c", Set generate_c, "Generate C"]
        (fun _ -> failwith "unexpected anonymous argument")
        "stubgen [-ml|-c]")
  in
  match !generate_ml, !generate_c with
  | false, false | true, true -> failwith "Exactly one of -ml and -c must be specified"
  | true, false ->
      Cstubs.write_ml Format.std_formatter ~prefix (module Mmdb_ffi_bindings.M)
  | false, true ->
      print_endline prologue;
      Cstubs.write_c Format.std_formatter ~prefix (module Mmdb_ffi_bindings.M)
