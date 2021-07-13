let () =
  print_char '"';
  try while true do
      print_string @@ match input_char stdin with
      | '"' -> "\\\""
      | c -> Char.escaped c
    done
  with End_of_file ->
    print_char '"'
