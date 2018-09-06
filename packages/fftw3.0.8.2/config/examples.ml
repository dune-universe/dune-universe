open Printf

let run p =
  let r = Sys.command p in
  if r <> 0 then (
    eprintf "Command %S terminated with code %d\n" p r;
    exit 1
  )

let archimedes_installed =
  Sys.command "ocamlfind query archimedes" = 0

let () =
  let d = Array.to_list(Sys.readdir ".") in
  let d = List.filter (fun f -> Filename.check_suffix f ".mlp") d in
  let arch = if archimedes_installed then "-D ARCHIMEDES_EXISTS" else "" in
  List.iter (fun f ->
      let f_out = String.sub f 0 (String.length f - 1) in
      run(Printf.sprintf "cppo %s %s -o %s" arch f f_out)
    )
    d
