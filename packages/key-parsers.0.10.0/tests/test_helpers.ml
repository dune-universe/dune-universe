let read_cstruct path =
  let ic = open_in path in
  let len = in_channel_length ic in
  let s = really_input_string ic len in
  close_in ic;
  Cstruct.of_string s

let fixture name =
  let path = Printf.sprintf "keys/%s" name in
  read_cstruct path
