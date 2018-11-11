open Earley

let int = parser
  | n:''[0-9]+'' -> int_of_string n
let op = parser
  | '+' -> (+)
  | '-' -> (-)
let expr = parser
  | n:int l:{op:op m:int -> (op,m)}* ->
      List.fold_left (fun acc (op,f) -> op acc f) n l

let parse =
  let blank = blank_regexp ''[ \t]*'' in
  handle_exception (parse_string ~filename:"arg" expr blank)

let _ =
  let cmd = Sys.argv.(0) in
  match Sys.argv with
  | [|_;s|] -> Printf.printf "%s = %i\n" s (parse s)
  | _       -> Printf.fprintf stderr "Usage: %s \"1 + 2 - 4\"\n" cmd
