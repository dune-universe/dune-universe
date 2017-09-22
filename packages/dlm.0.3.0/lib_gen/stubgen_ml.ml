let () = Cstubs.write_ml ~errno:Cstubs.return_errno ~concurrency:Cstubs.lwt_jobs ~prefix:"dlm_stubs_" Format.std_formatter (module Bindings.Make)
