let () =
  let env_variables = Dotenv.parse () in
  List.iter (fun (name, value) -> print_endline (Printf.sprintf "%s=%s" name value)) env_variables
