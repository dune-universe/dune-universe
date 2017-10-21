(* This scripts validates the output of alcotest *)
open Printf

let die msg =
  eprintf "%s\n" msg;
  exit 1

let get_lines filename =
  let lines = ref [] in
  let chan =
    try open_in filename with
    | _ -> die @@ "Could not open " ^ filename
  in
  try
    while true; do
      lines := input_line chan :: !lines
    done; []
  with End_of_file ->
    close_in chan;
    if List.length !lines = 0 then
      die "Test executable did not returned anything to normalize";
    List.rev !lines


let out =
  let lines = get_lines "main.output" in
  try List.nth lines 1 with
  | Failure msg ->
    ( Printf.eprintf "Could not read file 'main.output'\n";
      exit 1;
    )

open String

let format s =
  let s : string = trim s in
  sub s 0 ((index_from s 5 ':') -3)


let () = print_endline (format out)
