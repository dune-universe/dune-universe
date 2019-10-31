let () =
  try
    let rec loop () =
      read_line () |> Unidecode.decode_string |> print_endline ;
      loop ()
    in
    loop ()
  with End_of_file -> ()
