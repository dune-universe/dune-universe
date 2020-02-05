let () =
  print_string "let map_file = ";
  if String.compare Sys.ocaml_version "4.06.0" < 0
  then print_endline "Bigarray.Genarray.map_file"
  else print_endline "Unix.map_file"
