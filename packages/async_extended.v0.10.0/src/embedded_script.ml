(* Extraction and running of scripts embedded inside an executable. *)

open Core
module Process_in_this_dir = Process
open Async
module Process = Process_in_this_dir

let section_name ~script_name = ".jane." ^ script_name


let extract_script ~script_name ~into =
  let bash command =
    Process.backtick_status ()
      ~prog:"/bin/bash"
      ~args:["-c"; command]
  in
  let myself = Sys.executable_name in
  let section_name = section_name ~script_name in
  let readelf =
    Printf.sprintf "/usr/bin/readelf -S %s | /bin/grep %s"
      myself section_name
  in
  let objdump =
    Printf.sprintf
      "/usr/bin/objdump -s -j %s %s | /usr/bin/tail -n +5 | /usr/bin/xxd -r"
      section_name myself
  in
  bash readelf
  >>= fun (output, status) ->
  if not (Result.is_ok status) then
    failwithf "script '%s' not found in '%s' (stdout: %s, stderr: %s)"
      script_name myself output.Process.Output.stdout
      output.Process.Output.stderr ()
  else
    let script_path = into ^/ script_name in
    Writer.open_file script_path
    >>= fun writer ->
    bash objdump
    >>= fun (output, status) ->
    if not (Result.is_ok status)
    then
      failwithf "couldn't extract script '%s' from executable '%s': %s"
        script_name myself output.Process.Output.stderr ()
    else (
      Writer.write writer output.Process.Output.stdout;
      Writer.close writer
      >>= fun () ->
      Unix.chmod script_path ~perm:0o755
      >>| fun () ->
      script_path)


let run ~script_name ~args ~see_output =
  Unix.mkdtemp "/tmp/embedded_script."
  >>= fun temp_dir ->
  extract_script ~script_name ~into:temp_dir
  >>= fun script_path ->
  Process.backtick_status ()
    ~prog:script_path
    ~args
  >>= fun (output, status) ->
  Unix.unlink script_path
  >>= fun () ->
  Unix.rmdir temp_dir
  >>| fun () ->
  if see_output then (
    Writer.write (Lazy.force Writer.stdout) output.Process.Output.stdout;
    Writer.write (Lazy.force Writer.stderr) output.Process.Output.stderr;
  );
  if Result.is_ok status then `Ok else `Fail
