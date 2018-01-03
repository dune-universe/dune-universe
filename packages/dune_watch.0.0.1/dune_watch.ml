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

(* XXX should be configurable *)
let excludes = [ {|\.#|}
               ; {|_build/|}
               ; {|hg-check.*|}
               ; {|\.hg|}
               ; {|\.git|}
               ; {|\.cm.*|}
               ; {|\.annot|}
               ; {|\.o|}
               ; {|\.run|}
               ; {|\.omake.*|}
               ; {|\.tmp|}
               ]

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
  Lwt.catch loop & function _ -> do_;
    begin match%m ic#close with
    | Unix.WEXITED n ->
        reportf "exited %d" n; return ()
    | WSIGNALED n ->
        reportf "killed by signal %d" n; return ()
    | WSTOPPED n ->
        reportf "stopped by signal %d" n; return ()
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
  let excludes = List.concat (List.map (fun x -> ["--exclude"; x]) excludes) in
  ["fswatch"; "."; "--batch-marker"] @ excludes

let fswatch_com_array = Array.of_list fswatch_com

let watcher args =
  reportf "%s" (String.concat " " fswatch_com);
  let ic = Lwt_process.(open_process_in (List.hd fswatch_com, fswatch_com_array)) in
  let rec loop () = Lwt.do_;
    x <-- Lwt_io.read_line ic#stdout;
    match x with
    | "NoOp" ->
        ignore & trigger args; loop ()
    | _ ->
        reportf "%s" x;
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
  Lwt_main.run [%do
    builder args;
    watcher args
  ]
