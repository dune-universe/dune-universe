let exec b ppf s =
  let lb = Lexing.from_string s in
  try
    List.iter
      (fun phr -> if not (Toploop.execute_phrase b ppf phr) then raise Exit)
      (!Toploop.parse_use_file lb)
  with
  | Exit -> ()
  | x -> Errors.report_error ppf x

let exec_phrase s =
  exec false Format.std_formatter
    ("Format.pp_print_string Format.str_formatter (" ^ s ^ ")");
  Format.flush_str_formatter ()

let _ =
  Toploop.initialize_toplevel_env ();
  Toploop.input_name := "";
  ignore @@ Toploop.use_silently Format.std_formatter "test.ml";
  let str = exec_phrase "string_of_int (sum 5 3)" in
  Printf.printf "test:'%s'" str
