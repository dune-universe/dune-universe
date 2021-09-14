let () =
  let concurrency = Cstubs.unlocked in
  let prefix = Sys.argv.(2) in
  match Sys.argv.(1) with
  | "ml" ->
    Cstubs.write_ml ~concurrency Format.std_formatter ~prefix
      (module Mpg123_c_function_descriptions.Functions)
  | "c" ->
    print_endline "#include <mpg123.h>";
    Cstubs.write_c ~concurrency Format.std_formatter ~prefix
      (module Mpg123_c_function_descriptions.Functions)
  | s -> failwith ("unknown functions "^s)
;;
