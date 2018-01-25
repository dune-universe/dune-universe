open Unix
open Lwt

let (&) = (@@)

let failwithf fmt = Printf.ksprintf failwith fmt

let void x = x >>= fun _ -> return ()

let reportf fmt =
  let f s =
    prerr_string "dune_watch: ";
    prerr_endline s
  in
  Printf.ksprintf f fmt

let exec_process s = 
  let ic = open_process_in s in
  let rec loop rev_lines =
    match input_line ic with
    | exception End_of_file -> List.rev rev_lines
    | s -> loop (s :: rev_lines)
  in
  let lines = loop [] in
  ignore & close_process_in ic;
  lines

module Conf = struct
  open Camlon
  open Ocaml_conv.Default
  type uname = Linux | Darwin [@@deriving conv{ocaml}]
  type t = { excludes : string list } [@@deriving conv{ocaml}]

  let uname = match exec_process "uname" with
    | ["Linux"] -> Linux
    | ["Darwin"] -> Darwin
    | _ -> assert false
      
  let default =
    { excludes = [ {|/\.#|}
                 ; {|_build|}
                 ; {|hg-check.*|}
                 ; {|\.hg|}
                 ; {|\.git|}
                 ; {|\.cm.*|}
                 ; {|\.annot$|}
                 ; {|\.o$|}
                 ; {|\.a$|}
                 ; {|\.run$|}
                 ; {|\.omake.*|}
                 ; {|\.tmp$|}
                 ; {|~$|}
                 ; {|/#[^#]*#$|}
                 ]
    }

  let conf =
    let name = "_dune_watch" in
    if Sys.file_exists name then begin
      reportf "Using conf from %s" name;
      match Ocaml.load_with t_of_ocaml name with
      | Ok [conf] -> conf
      | _ -> failwithf "Failed to decode %s" name
    end else default
end

let modified = ref false
let running = ref false

let rec builder args =
  let com = "jbuilder", (Array.of_list & "jbuilder" :: "build" :: args) in
  let com_string = String.concat " " (Array.to_list & snd com) in
  reportf "start: %s" com_string;
  modified := false;
  running := true;
  let ic = Lwt_process.(open_process_in com) in
  let rec loop () = do_;
    x <-- Lwt_io.read_line ic#stdout;
    (); prerr_endline x;
    loop ()
  in
  Lwt.catch loop & fun _ -> do_;
    begin match%m ic#close with
    | Unix.WEXITED n ->
        reportf "exited %d" n;
        return ()
    | WSIGNALED n ->
        reportf "killed by signal %d" n;
        return ()
    | WSTOPPED n ->
        reportf "stopped by signal %d" n;
        return ()
    end;
    if !modified then builder args
    else begin
      running := false;
      reportf "end";
      Lwt.return ()
    end

let trigger args =
  modified := true;
  if not !running then builder args else return ()

let fswatch_com =
  let open Conf in
  match Conf.uname with
  | Darwin ->
      let excludes = List.(concat & map (fun x -> ["--exclude"; x]) conf.excludes) in
      ["fswatch"; "."; "--batch-marker"] @ excludes
  | Linux ->
      (* inotifywatch cannot take more than one --exclude! *)
      let excludes = String.concat "|" conf.excludes in
      (* XXX close_write may not be sufficient. *)
      ["inotifywait"; "-mr"; "."; "--exclude"; excludes; "-e"; "close_write"]

let fswatch_com_array = Array.of_list fswatch_com

let watcher args =
  let ic = Lwt_process.(open_process_in (List.hd fswatch_com, fswatch_com_array)) in
  let rec loop () = do_;
    x <-- Lwt_io.read_line ic#stdout;
    let do_report, do_trigger = match Conf.uname with
      | Darwin -> let b = x = "NoOp" in not b, b
      | _ -> true, true
    in
    (); if do_report then reportf "%s" x;
    (); if do_trigger then ignore & trigger args;
    loop ()
  in
  Lwt.catch loop & function _ -> void ic#close

let () =
  let rev_args = ref [] in
  let rest s = rev_args := s :: !rev_args in
  Arg.(parse [ "--", Rest rest, "pass the rest of options to jbuilder build" ]
         (failwithf "dune_watch does not take anonymous arguments: %s")
         "dune_watch <options> -- <options for jbuilder build>");
  let args = List.rev !rev_args in
  reportf "%s" (String.concat " " fswatch_com);
  Lwt_main.run [%do
    builder args;
    watcher args
  ]
