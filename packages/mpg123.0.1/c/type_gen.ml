let () =
  print_endline "#include <mpg123.h>";
  Cstubs_structs.write_c Format.std_formatter
    (module Mpg123_c_type_descriptions.Types)
;;
