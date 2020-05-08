let () =
  let ic = open_in_bin "ic.ml" in
  [%defer close_in ic];
  let length = in_channel_length ic in
  let bytes = really_input_string ic length in
  print_endline bytes
