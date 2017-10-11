(* This scripts validates the output of alcotest *)


let get_lines filename =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := input_line chan :: !lines
    done; []
  with End_of_file ->
    close_in chan;
    List.rev !lines


let out =
  let lines = get_lines "main.output" in
  List.nth lines 1

open String

let format s =
  let s : string = trim s in
  sub s 0 ((index_from s 5 ':') -3)


let () = print_endline (format out)
