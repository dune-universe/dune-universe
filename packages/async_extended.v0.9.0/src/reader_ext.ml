open Core
module Process_in_this_dir = Process
open Async
module Process = Process_in_this_dir

include Reader

let input_sexps reader =
  Deferred.create (fun ivar ->
    let rec loop accum =
      upon (Reader.read_sexp reader)
        (function
        | `Ok s -> loop (s :: accum)
        | `Eof -> Ivar.fill ivar accum
        )
    in
    loop []
  )

let gzip_is_ok = function
  | Error (`Signal s) when s = Signal.pipe -> true
  | status -> Result.is_ok status

let with_input_from_process ~prog ~args ~f =
  let is_ok status =
    match status with
    | Error (`Signal s) when Signal.(equal pipe) s -> true
    | _ -> Result.is_ok status
  in
  Process.open_in ~is_ok ~prog ~args ()
  >>= fun {Process.Output.stdout; stderr} ->
  Monitor.protect
    ~finally:(fun () -> Reader.close stderr)
    (fun () ->
       let act_on_input =
         Monitor.protect
           ~finally:(fun () -> Reader.close stdout)
           (fun () -> f stdout)
       in
       Deferred.both act_on_input (Reader.contents stderr))
  >>| fun (res, stderr) ->
  if not (String.is_empty stderr) then
    raise_s [%message
      "with_input_from_process: failed reading input"
        (prog : string) (stderr : string)];
  res
;;

let with_gzip_file file ~f =
  with_input_from_process ~prog:"gunzip" ~args:["--to-stdout"; file] ~f

let with_hadoop_gzip_file ~hadoop_file f =
  with_input_from_process ~prog:"bash"
    ~args:["-c"; sprintf "qdfsCmd.exe cat %s | gunzip -c" hadoop_file ] ~f

let with_xzip_file file ~f =
  with_input_from_process ~prog:"xzcat" ~args:["--stdout"; file] ~f

let open_gzip_file file =
  Process.open_in ~is_ok:gzip_is_ok ~prog:"gunzip" ~args:["--to-stdout"; file] ()
  >>= fun {Process.Output.stdout = stdout; stderr} ->
  Reader.close stderr
  >>= fun () ->
  return stdout
