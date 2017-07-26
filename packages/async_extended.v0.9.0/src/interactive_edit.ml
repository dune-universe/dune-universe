open Core
module Process_in_this_dir = Process
open Async
module Process = Process_in_this_dir

let edit_file ?success_message ~post_hook ~path () =
  let editor = Core_extended.Std.Sys_utils.get_editor_exn () in
  Unix.mkdtemp "/tmp/interactive-edit."
  >>= fun temp_dir ->
  Process.backtick_status ()
    ~prog:"/bin/cp"
    ~args:[path; temp_dir]
  >>= fun (_output, status) ->
  if not (Result.is_ok status) then
    Unix.rmdir temp_dir
    >>= fun () ->
    failwith "couldn't copy config to temporary directory"
  else
    let temp_file = temp_dir ^/ (Filename.basename path) in
    let cleanup () =
      Unix.unlink temp_file
      >>= fun () ->
      Unix.rmdir temp_dir
    in
    Core.Printf.printf "editing: %s\n%!" path;
    let cmd = Printf.sprintf "%s %s" editor temp_file in
    Unix.system cmd
    >>= fun status ->
    if not (Result.is_ok status) then
      cleanup () >>| fun () -> Core.Printf.printf "not committing changes.\n%!"
    else
      Reader.file_contents temp_file
      >>= fun contents ->
      Reader.file_contents path
      >>= fun contents' ->
      if String.equal contents contents' then
        cleanup () >>| fun () -> Core.Printf.printf "no changes made.\n%!"
      else
        Process.backtick_status ()
          ~prog:"/bin/cp"
          ~args:["-f"; temp_file; path]
        >>= fun (_output, status) ->
        if not (Result.is_ok status) then
          cleanup ()
          >>|
          fun () -> failwith "couldn't copy edited config to repo"
        else
          cleanup ()
          >>= fun () ->
          post_hook ()
          >>| fun () ->
          match success_message with
          | None -> ()
          | Some msg -> Core.Printf.printf "%s\n%!" msg
