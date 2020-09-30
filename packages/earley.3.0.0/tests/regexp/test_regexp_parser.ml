open Earley_core.Regexp

let _ =
  let (re, _) = regexp_from_string Sys.argv.(1) in
  Printf.printf "%S ⇒ %a\n%!" Sys.argv.(1) print_regexp re
