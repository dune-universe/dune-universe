open Core

let debug = false

(* This is set if we have examined the core_profiler env variable. *)
let core_profiler_env_table_opt : string String.Table.t option ref = ref None

let print_help_and_exit () =
  eprintf "%s\n%!" Core_profiler_disabled.Intf.core_profiler_env_help_string;
  exit(1)
;;

let set_env_table_from_string str =
  if debug then printf "profiler var = %s\n" str;
  let tbl = String.Table.create () in
  List.iter (String.split str ~on:',') ~f:(fun name_val ->
    if not (String.is_empty name_val) then begin
      match String.lsplit2 name_val ~on:'=' with
      | None -> print_help_and_exit ()
      | Some (name, value) ->
        String.Table.set tbl ~key:name ~data:value
    end);
  core_profiler_env_table_opt := Some tbl;
  Some tbl

(** entirely suppress checking for the environment variable if we are in a inline test or
    inline benchmark *)
let () =
  match Array.to_list Sys.argv with
  | _name :: "inline-test-runner" :: _rest
  | _name :: "-benchmarks-runner" :: _rest
    -> ignore (set_env_table_from_string "" : string String.Table.t option )
  | _cmd -> ()

let get_env_table_opt () =
  match !core_profiler_env_table_opt with
  | Some tbl -> Some tbl
  | None ->
    (match Sys.getenv "CORE_PROFILER" with
     | None -> None
     | Some str -> set_env_table_from_string str)


let get_var str =
  match get_env_table_opt () with
  | None ->
    if debug then printf "no table\n%!";
    None
  | Some tbl ->
    let res = String.Table.find tbl str in
    (match res with
    | Some v ->
      if debug then printf "var %s is %s\n%!" str v;
    | None ->
      if debug then printf "var %s is not set\n%!" str);
    res

let check_safety_exn () =
  if Option.is_none (get_env_table_opt ())
  then print_help_and_exit ()

let don't_require_core_profiler_env () =
  if Option.is_none (get_env_table_opt ())
  then ignore (set_env_table_from_string "" : string String.Table.t option )
