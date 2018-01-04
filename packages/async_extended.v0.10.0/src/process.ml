open Core
open Async

module Exit_or_signal_or_stop = Unix.Exit_or_signal_or_stop
module Fd = Unix.Fd
module Async_unix = Unix
module Unix = Core.Unix

module Output = struct
  type 'a t = {
    stdout: 'a;
    stderr: 'a
  }
end

let term_or_kill pid_spec =
  (* catch EPERM which happens when user doesn't have permission to kill the process. we
     can't really do anything at this point, and library users can't catch the exception,
     so ignore. should probably refactor to let library users handle this themselves. *)
  match
    try
      (* SIGTERM is handled by the process: usually the process will stop and perform any
         required cleanup before terminating. *)
      (Signal.send Signal.term pid_spec :> [`No_such_process | `Ok | `Permission_denied])
    with Unix.Unix_error (EPERM, _, _) -> `Permission_denied
  with
  | `No_such_process -> ()
  | `Permission_denied -> ()
  | `Ok ->
    after (sec 1.) >>> fun () ->
    (* SIGKILL is handled by the OS: the process is removed from the scheduler and the
       memory freed and the process is not given the opportunity to perform cleanup. *)
    ignore (Signal.send Signal.kill pid_spec)
;;

module Kill_at_shutdown : sig
  val register : Signal.pid_spec -> unit
  val unregister : Signal.pid_spec -> unit
end = struct
  let kill_me = ref Set.Poly.empty

  let register pid_spec = kill_me := Set.add !kill_me pid_spec
  let unregister pid_spec = kill_me := Set.remove !kill_me pid_spec

  let () =
    Shutdown.at_shutdown (fun () ->
      Set.iter !kill_me ~f:term_or_kill;
      Deferred.unit)
  ;;
end

(*
   As it turns out, forking is tricky... here are some of the special cases that
   this function attempts to take out of the hands of the user.  Read this list carefully
   before messing around with this function.  In particular, there are non obvious cases
   where the order of calls is important:
   - if [f ()] throws an exception, make sure the file descriptors are closed and the
     forked program is killed
   - if the forked program exits before [f ()], make sure it is reaped quickly, and that
     the pipes are closed, regardless of the state of f
   - after you fork a child, it will have duped the endpoints of your pipes, and the
     parent should close the local endpoints
   - if the program calling [f ()] exits while f is running, make sure that the forked
     child is killed
*)
let create ?kill:kill_def ?(kill_how=`by_pid)
    ~prog ~args ?(env=`Extend []) ?working_dir ?(stdin = "") ~f () =
  let stdin_text = stdin in
  let process_exited = ref false in
  let kill ~pid_spec = if not !process_exited then term_or_kill pid_spec in
  In_thread.syscall_exn ~name:"create_process_env" (fun () ->
    Unix.create_process_env ~prog ~args ~env ?working_dir ())
  >>= fun { Unix.Process_info. pid; stdin; stdout; stderr } ->
    let pid_spec = (match kill_how with `by_pid -> `Pid pid | `by_group -> `Group pid) in
    Kill_at_shutdown.register pid_spec;
    let create_fd name file_descr =
      Fd.create Fd.Kind.Fifo file_descr
        (Info.create "child process" (name, `pid pid, `prog prog, `args args)
           ([%sexp_of:
               string * [ `pid of Pid.t ] * [ `prog of string ] * [ `args of string list ]
            ]))
    in
    let stdin  = Writer.create (create_fd "stdin"  stdin ) in
    let stdout = Reader.create (create_fd "stdout" stdout) in
    let stderr = Reader.create (create_fd "stderr" stderr) in
    let cleanup () =
      Writer.close stdin                 >>= fun () ->
      Reader.close stdout                >>= fun () ->
      Reader.close stderr                >>| fun () ->
      after (sec 1.0) >>> fun () ->
      kill ~pid_spec;
      Kill_at_shutdown.unregister pid_spec
    in
    let process_status =
      Async_unix.waitpid pid
      >>| fun status ->
      process_exited := true;
      status
    in
    Stream.iter (Monitor.detach_and_get_error_stream (Writer.monitor stdin)) ~f:(fun _ -> ());
    Writer.write stdin stdin_text;
    Monitor.try_with (fun () ->
      Option.iter kill_def ~f:(fun v -> v >>> fun () -> kill ~pid_spec);
      Writer.flushed stdin >>= fun _ -> (
        f pid ~stdin ~stdout ~stderr
      )
    )
    >>= fun r ->
    cleanup ()
    >>= fun () ->
    match r with
    | Ok v -> process_status >>| (fun ps -> (v, ps))
    | Error e ->
      raise (Monitor.extract_exn e)
;;

type file_descr = Unix.File_descr.t

let create_fds'
      ~kill:kill_def ?(kill_how=`by_pid)
      ~prog ~args ?env ~stdin ~stdout ~stderr ~f:caller_f () =
  match Unix.fork () with
  | `In_the_child ->
    begin
      try
        Unix.dup2 ~src:stdin ~dst:Unix.stdin;
        Unix.dup2 ~src:stdout ~dst:Unix.stdout;
        Unix.dup2 ~src:stderr ~dst:Unix.stderr;
        never_returns (Unix.exec ?env ~prog ~argv:(prog :: args) ());
      with
      | _ ->
        Core.Printf.eprintf "exec failed: %s\n%!" prog;
        Unix.exit_immediately 1
    end
  | `In_the_parent pid ->
    let pid_spec = (match kill_how with `by_pid -> `Pid pid | `by_group -> `Group pid) in
    Kill_at_shutdown.register pid_spec;
    let process_exited = ref false in
    let kill_child () =
      if not !process_exited then term_or_kill pid_spec;
    in
    (kill_def >>> kill_child);
    caller_f pid >>= fun caller_result ->
    let process_status =
      Async_unix.waitpid pid
      >>| fun status ->
      process_exited := true;
      Kill_at_shutdown.unregister pid_spec;
      (caller_result,status)
    in
    process_status
;;

let create_fds ~kill ?kill_how ~prog ~args ?env ~stdin ~stdout ~stderr ~f () =
  create_fds' ~kill ?kill_how ~prog ~args ?env ~stdin ~stdout ~stderr
    ~f:(fun pid -> f pid; Deferred.unit) ()
  >>| fun ((), eos) ->
  eos
;;

let open_in ?(is_ok = Result.is_ok) ?kill ~prog ~args () =
  let cmd = sprintf "%s %s" prog (String.concat args ~sep:" ") in
  let readers = Ivar.create () in
  let status =
    create ?kill ~prog ~args () ~f:(fun _pid ~stdin ~stdout ~stderr ->
      Ivar.fill readers (stdout, stderr);
      Writer.close stdin >>= fun () ->
        Deferred.all_unit [ Reader.close_finished stdout; Reader.close_finished stderr ]
    )
  in
  upon status (fun ((), status) ->
    let killed =
      match kill with
      | Some v -> Deferred.peek v = Some ()
      | None -> false
    in
    if not (is_ok status) && (not killed) then
      failwithf "Process.open_in '%s' exited with status '%s'"
        cmd (Unix.Exit_or_signal.to_string_hum status) ());
  Ivar.read readers
  >>| fun (stdout, stderr) ->
  { Output. stdout; stderr }
;;

type 'a backtick =
  ?kill:unit Deferred.t
  -> ?env:Unix.env
  -> prog:string
  -> args:string list
  -> ?working_dir:string
  -> ?stdin:string
  -> unit
  -> 'a

module Backtick : sig
  type 'a t = 'a backtick

  val map : 'a t -> f:('a -> 'b) -> 'b t
end = struct
  type 'a t = 'a backtick

  let map t ~f ?kill ?env ~prog ~args ?working_dir ?stdin () =
    f (t ?kill ?env ~prog ~args ?working_dir ?stdin ())
end

let backtick_status ?kill ?env ~prog ~args ?working_dir ?(stdin = "") () =
  create () ?kill ~prog ~args ?env ?working_dir ~stdin ~f:(fun _pid ~stdin ~stdout ~stderr ->
    Writer.close stdin
    >>= fun () ->
    Deferred.both (Reader.contents stdout) (Reader.contents stderr)
  ) >>| fun ((stdout, stderr), status) ->
  ({ Output.stdout; stderr; }, status)
;;

let backtick =
  Backtick.map backtick_status
    ~f:(fun d -> d >>| fun (output, _status) -> output)
;;

module Lines_or_sexp = struct
  type t =
  | Lines of string list
  | Sexp of Sexp.t
  [@@deriving sexp_of]

  let create string =
    try Sexp (Sexp.of_string string)
    with _ -> Lines (String.split ~on:'\n' string)
  ;;
end

module Command_failed = struct
  type t =
    { prog : string;
      args : string list;
      status: Unix.Exit_or_signal.t;
      stdout : Lines_or_sexp.t;
      stderr : Lines_or_sexp.t;
    }
  [@@deriving sexp_of]

  exception E of t [@@deriving sexp]
end

let backtick_new ?kill ?env ~prog ~args ?working_dir ?stdin () =
  backtick_status ?kill ?env ~prog ~args ?working_dir ?stdin ()
  >>| fun (output, status) ->
  let stdout = output.Output.stdout in
  if Result.is_ok status then
    Ok stdout
  else
    Error (Command_failed.E
             { Command_failed.
               prog = prog;
               args = args;
               stdout = Lines_or_sexp.create stdout;
               stderr = Lines_or_sexp.create output.Output.stderr;
               status = status;
             })
;;

let backtick_new_exn =
  Backtick.map backtick_new ~f:(fun res ->
    res >>| function
      | Ok stdout -> stdout
      | Error exn -> raise exn)
;;
