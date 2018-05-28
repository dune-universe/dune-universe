open Base
open Stdio

let run p =
  let r = Caml.Sys.command p in
  if r <> 0 then (
    eprintf "Command %S terminated with code %d\n" p r;
    Caml.exit 1
  )

let archimedes_installed =
  Caml.Sys.command "ocamlfind query archimedes" = 0

let () =
  let d = Array.to_list(Caml.Sys.readdir ".") in
  let d = List.filter d ~f:(fun f -> String.is_suffix f ".mlp") in
  let arch = if archimedes_installed then "-D ARCHIMEDES_EXISTS" else "" in
  List.iter d ~f:(fun f ->
      let f_out = String.sub f 0 (String.length f - 1) in
      run(Printf.sprintf "cppo %s %s -o %s" arch f f_out)
    )
