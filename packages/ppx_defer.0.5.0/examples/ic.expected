let () =
  set_binary_mode_out stdout true;
  let ic = open_in_bin "ic.ml" in
  [%defer close_in ic];
  let length = in_channel_length ic in
  let bytes = really_input_string ic length in
  print_string bytes
