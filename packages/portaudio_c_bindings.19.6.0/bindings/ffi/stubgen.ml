let prefix = "caml_portaudio"

let c_headers = {|
#include <portaudio.h>
|}

let () =
  let generate_ml, generate_c = ref false, ref false in
  let () =
    Arg.(parse [ ("-ml", Set generate_ml, "Generate ML");
                 ("-c", Set generate_c, "Generate C") ])
      (fun _ -> failwith "unexpected anonymous argument")
      "stubgen [-ml|-c]"
  in
  match !generate_ml, !generate_c with
  | false, false
  | true, true ->
    failwith "Exactly one of -ml and -c must be specified"
  | true, false ->
    Cstubs.write_ml Format.std_formatter ~concurrency:Cstubs.unlocked ~prefix (module Portaudio_ffi_stubs.Make)
  | false, true ->
    print_endline c_headers;
    Cstubs.write_c Format.std_formatter ~concurrency:Cstubs.unlocked ~prefix (module Portaudio_ffi_stubs.Make)
