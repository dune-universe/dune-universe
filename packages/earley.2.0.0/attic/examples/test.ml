open Decap

let blank = blank_regexp ''[ \t\n\r]*''

let test =
  parser
  | {"a" | "a" "b" } "c"

let _ =
  if Unix.((fstat (descr_of_in_channel Pervasives.stdin)).st_kind = S_REG)
  then handle_exception (fun () ->
		     parse_channel test blank stdin
		     ) ()
  else
    try
      while true do
	handle_exception (fun () ->
	  Printf.printf ">> %!";
	  parse_string test blank (input_line stdin)) ()
      done
  with End_of_file -> ()
