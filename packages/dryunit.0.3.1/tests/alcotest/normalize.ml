(* This scripts validates the output of alcotest *)


let out =
  let c = open_in "main.output" in
  let line = input_line c in
  close_in c;
  line

open String

let format s =
  let s : string = trim s in
  let s : string = sub s 0 25 in
  s ^ "}"

let () = print_endline (format out)
