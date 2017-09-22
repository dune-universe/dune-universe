let c_headers = "#include <stdint.h>\n#include <stdlib.h>\n#include <libdlm.h>"
let () =
  Format.fprintf Format.std_formatter "%s@\n" c_headers;
  Cstubs_structs.write_c Format.std_formatter (module Bindings_structs_lib.Bindings_structs.Make)
