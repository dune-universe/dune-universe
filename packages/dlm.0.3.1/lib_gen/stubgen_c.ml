let c_headers = "#include <stdint.h>\n#include <stdlib.h>\n#include <libdlm.h>"

let () =
  Format.fprintf Format.std_formatter "%s@\n" c_headers;
  Cstubs.write_c ~errno:Cstubs.return_errno ~concurrency:Cstubs.lwt_jobs
    ~prefix:"dlm_stubs_" Format.std_formatter (module Dlm_bindings.Bindings.Make)
