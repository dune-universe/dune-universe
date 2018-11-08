open Printf

let read_all ch =
  let len = ref 0 in
  let b = Bytes.create 4096 in
  let buf = Buffer.create 4096 in
  while len := input ch b 0 4096; !len > 0 do
    Buffer.add_subbytes buf b 0 !len
  done;
  Buffer.contents buf

let () =
  let fh = Bwrap.open_process_in Bwrap.bare "/usr/bin/env" [] in
  if Bwrap.close_process_in fh = Unix.WEXITED 1 then
    printf "Good, exit code testifying that the program does not exists.\n%!"
  else assert false

let sandbox ?(conf = Bwrap.conf ()) cmd args ~f =
  let a = if args = [] then "" else " " ^ String.concat " " args in
  printf "[31;1m%s%s[0m: %!" cmd a;
  let fh = Bwrap.open_process_in conf cmd args in
  let out = read_all fh in
  match Bwrap.close_process_in fh with
  | Unix.WEXITED 0 -> f out
  | Unix.WEXITED i ->
     printf "- Process terminated with %i\n- stdout: %S\n" i out
  | Unix.WSIGNALED s ->
     printf "- Process killed by signal %i\n- stdout: %S\n" s out
  | Unix.WSTOPPED s ->
     printf "- Process stopped by signal %i\n- stdout: %S\n" s out

let () =
  let run ?conf cmd args =
    sandbox ?conf cmd args ~f:(fun out ->
        printf "%s" out;
        if out = "" || out.[String.length out - 1] <> '\n' then printf "\n"
      ) in
  run "env" [] ~conf:Bwrap.(conf() |> setenv "ADDED_VAR" "$HOME");
  run "pwd" [];
  run "ls" ["-lp"];
  run "ls" ["/var"];
  run "hostname" [];
  run "echo" ["-e"; "Hello\\tworld"]
