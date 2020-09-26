
let ok = ref true
let test file =
  Printf.eprintf "test file '%s'\n%!" file;
  let code = Sys.command ("../src/bin/smtlib_cat.exe -q " ^ file) in
  if code <> 0 then (
    Printf.eprintf "> file '%s': code=%d\n%!" file code;
    ok := false
  )

let () =
  for i = 1 to Array.length Sys.argv - 1 do
    test Sys.argv.(i)
  done;
  if not !ok then exit 1
