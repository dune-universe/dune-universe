let () =
  let json = Jsonxt.Basic.of_string "[1,2,3]" in
  print_endline (Jsonxt.Utilities.json_to_string_repr json);;
