open Unix
open Lwt

module Utils = struct
  
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

end

open Utils

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
                 ; {|\.install$|}
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
let build_root = ref "."

module Builder = struct

 let re_entering_directory = Re.compile & Re.Pcre.re "^Entering directory '(.*)'$"

 let rec builder args =
   let com = "jbuilder", (Array.of_list & "jbuilder" :: "build" :: "--dev" :: args) in
   let com_string = String.concat " " (Array.to_list & snd com) in
   reportf "start: %s" com_string;
   modified := false;
   running := true;
   let proc = Lwt_process.(open_process_full com) in
   let rec loop () = do_;
     x <-- Lwt.pick [ Lwt_io.read_line proc#stdout
                    ; Lwt_io.read_line proc#stderr 
                    ];
     (); prerr_endline x;
     (); begin match Re.exec_opt re_entering_directory x with
       | None -> ()
       | Some gs -> 
           let d = Re.Group.get gs 1 in
           prerr_endline ("Build root: " ^ d);
           build_root := d
     end;
     loop ()
   in
   Lwt.catch loop & fun exn -> do_;
     (); reportf "exception: %s" & Printexc.to_string exn;
     begin match%m proc#close with
     | Unix.WEXITED n ->
         reportf "exited %d: %s" n com_string;
         return ()
     | WSIGNALED n ->
         reportf "killed by signal %d: %s" n com_string;
         return ()
     | WSTOPPED n ->
         reportf "stopped by signal %d: %s" n com_string;
         return ()
     end;
     if !modified then begin do_;
       (); reportf "file modification detected.";
       builder args
     end else begin
       running := false;
       reportf "end";
       Lwt.return ()
     end

 let trigger args =
   modified := true;
   if not !running then builder args else return ()
end

module FSWatch = struct
  (* XXX The command may not exit.  We need to check its existence first. *)
  let fswatch_com =
    let open Conf in
    match Conf.uname with
    | Darwin ->
        let excludes = List.(concat & map (fun x -> ["--exclude"; x]) conf.excludes) in
        ["fswatch"; "."; "--batch-marker"] @ excludes
    | Linux ->
        (* inotifywait cannot take more than one --exclude! *)
        let excludes = String.concat "|" conf.excludes in
        (* XXX close_write may not be sufficient. *)
        ["inotifywait"; "-mr"; "."; "--exclude"; excludes; "-e"; "close_write"; "-q"]
  
  let fswatch_com_array = Array.of_list fswatch_com
  
  let watcher args =
    reportf "%s" (String.concat " " fswatch_com);
    let proc = Lwt_process.(open_process_full (List.hd fswatch_com, fswatch_com_array)) in
    let rec loop () = do_;
      x <-- Lwt.pick [ Lwt.map (fun x -> `Stdout x) & Lwt_io.read_line proc#stdout
                     ; Lwt.map (fun x -> `Stderr x) & Lwt_io.read_line proc#stderr 
                     ];
      let do_report, do_trigger = match Conf.uname with
      | Darwin -> let b = x = `Stdout "NoOp" in not b, b
      | _ -> true, true
      in
      (); if do_report then begin match x with
        | `Stdout x -> reportf "%s" x
        | `Stderr x -> reportf "(err) %s" x
      end;
      (); if do_trigger then ignore & Builder.trigger args;
      loop ()
    in
    Lwt.catch loop & function 
      | End_of_file as e ->
          reportf "watcher raised an exception: %s.  Do you have %s installed?" (Printexc.to_string e) (List.hd fswatch_com);
          void proc#close
      | e ->
          reportf "watcher raised an exception: %s" (Printexc.to_string e);
          void proc#close
end

let () =
  let rev_args = ref [] in
  let rest s = rev_args := s :: !rev_args in
  Arg.(parse [ "--", Rest rest, "pass the rest of options to jbuilder build" ]
         (failwithf "dune_watch does not take anonymous arguments: %s")
         "dune_watch <options> -- <options for jbuilder build>");
  let args = List.rev !rev_args in
  Lwt_main.run [%do
    Builder.builder args;
    FSWatch.watcher args
  ]
