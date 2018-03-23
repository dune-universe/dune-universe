open Core
open Async

module Command = struct
  module Invocation = struct
    type t = Sexp | Bin_io of Rpc.Connection.t
  end

  module Stateful = struct
    module type T = sig
      type query    [@@deriving of_sexp]
      type response [@@deriving sexp_of]
      type state
      val rpc : (query, response) Rpc.Rpc.t
      val implementation : state -> query -> response Deferred.t
    end

    module type T_conv = sig
      include Versioned_rpc.Callee_converts.Rpc.S
      type state
      val name : string
      val query_of_sexp    : Sexp.t -> query
      val sexp_of_response : response -> Sexp.t
      val implementation : state -> query -> response Deferred.t
    end

    module type T_pipe = sig
      type query    [@@deriving of_sexp]
      type response [@@deriving sexp_of]
      type error    [@@deriving sexp_of]
      type state
      val rpc : (query, response, error) Rpc.Pipe_rpc.t
      val implementation
        :  state
        -> query
        -> (response Pipe.Reader.t, error) Result.t Deferred.t
    end

    type 'state t = [
      | `Plain      of (module T      with type state = 'state)
      | `Plain_conv of (module T_conv with type state = 'state)
      | `Pipe       of (module T_pipe with type state = 'state)
    ]

    let lift (type a) (type b) (t : a t) ~(f : b -> a) : b t =
      match t with
      | `Plain (module M) ->
        `Plain (module struct
          include (M : T with type state := a)
          type state = b
          let implementation state query = implementation (f state) query
        end)
      | `Plain_conv (module M) ->
        `Plain_conv (module struct
          include (M : T_conv with type state := a)
          type state = b
          let implementation state query = implementation (f state) query
        end)
      | `Pipe (module M) ->
        `Pipe (module struct
          include (M : T_pipe with type state := a)
          type state = b
          let implementation state query = implementation (f state) query
        end)
  end

  module type T      = Stateful.T      with type state := Invocation.t
  module type T_conv = Stateful.T_conv with type state := Invocation.t
  module type T_pipe = Stateful.T_pipe with type state := Invocation.t

  type t = [
    | `Plain      of (module T)
    | `Plain_conv of (module T_conv)
    | `Pipe       of (module T_pipe)
  ]

  let stateful (rpcs : Invocation.t Stateful.t list) = (rpcs :> t list)

  let menu impls =
    match
      Map.Poly.of_alist
        (List.concat_map impls ~f:(fun impl ->
           match impl with
           | `Plain plain ->
             let module T = (val plain : T) in
             [((Rpc.Rpc.name T.rpc, Rpc.Rpc.version T.rpc), impl)]
           | `Plain_conv x ->
             let module T = (val x : T_conv) in
             let versions = Set.to_list @@ T.versions () in
             List.map versions ~f:(fun version -> ((T.name, version), impl))
           | `Pipe pipe ->
             let module T = (val pipe : T_pipe) in
             [((Rpc.Pipe_rpc.name T.rpc, Rpc.Pipe_rpc.version T.rpc), impl)]
         ))
    with
    | `Ok map -> map
    | `Duplicate_key (name, version) ->
      failwithf "multiple implementations of rpc (%s %d)" name version ()

  let implementations ?log_not_previously_seen_version
    : t -> Invocation.t Rpc.Implementation.t list
    = function
      | `Plain (module T) ->
        [Rpc.Rpc.implement T.rpc T.implementation]
      | `Plain_conv (module T) ->
        T.implement_multi ?log_not_previously_seen_version
          (fun s ~version:_ q -> T.implementation s q)
      | `Pipe (module T) ->
        [Rpc.Pipe_rpc.implement T.rpc T.implementation]

  type call = {
    rpc_name : string;
    version : int;
    query : Sexp.t;
  } [@@deriving sexp]

  let write_sexp w sexp = Writer.write_sexp w sexp; Writer.newline w

  (** This function returns [stdin] and [stdout] as similar to the original [Reader.stdin]
      and [Writer.stdout] as possible, except they should have new file descriptor numbers
      (greater than 2) to avoid the other parts of the program writing there by accident.

      It also changes file descriptors such that after this function [Reader.stdin] is
      reading from /dev/null and [Writer.stdout] is writing to stderr.
  *)
  let claim_stdin_and_stdout_for_exclusive_use () =
    let same_fd fd1 fd2 =
      Int.(=)
        (Core.Unix.File_descr.to_int fd1)
        (Core.Unix.File_descr.to_int fd2)
    in
    let equivalent_fd fd1 fd2 =
      (* this is the same check [Writer] does when sharing the writer between stderr
         and stdout *)
      let dev_and_ino fd =
        let stats = Core.Unix.fstat fd in
        (stats.st_dev, stats.st_ino)
      in
      same_fd fd1 fd2 || dev_and_ino fd1 = dev_and_ino fd2
    in
    let stdin  = Lazy.force Reader.stdin  in
    let stdout = Lazy.force Writer.stdout in
    let stderr = Lazy.force Writer.stderr in
    let module Standard_fd = struct
      let stdin  = Core.Unix.File_descr.of_int 0
      let stdout = Core.Unix.File_descr.of_int 1
      let stderr = Core.Unix.File_descr.of_int 2
    end
    in
    assert (same_fd (Fd.file_descr_exn (Reader.fd stdin)) Standard_fd.stdin);
    (* Async has a special hack where if file descriptors 1 and 2 happen to point to the
       same file/device (for example when running in a tty) then it only creates one
       writer (ignoring file descriptor 2) that's used for both [Writer.stderr] and
       [Writer.stdout]. In this situation [same_fd] will be false, but [equivalent_fd]
       will be true and that should be enough to keep sanity below. *)
    assert (equivalent_fd (Fd.file_descr_exn (Writer.fd stdout)) Standard_fd.stdout);
    assert (equivalent_fd (Fd.file_descr_exn (Writer.fd stderr)) Standard_fd.stderr);
    let make_a_copy_of_stdin_and_stdout () =
      let dupped_stdin  = Core.Unix.dup Standard_fd.stdin  in
      Core.Unix.set_close_on_exec dupped_stdin;
      let dupped_stdout = Core.Unix.dup Standard_fd.stdout in
      Core.Unix.set_close_on_exec dupped_stdout;
      assert (Core.Unix.File_descr.to_int dupped_stdin  > 2);
      assert (Core.Unix.File_descr.to_int dupped_stdout > 2);
      let create_fd ~similar_to fd =
        Fd.create (Fd.kind similar_to) fd (Fd.info similar_to)
      in
      let stdin =
        Reader.create (create_fd ~similar_to:(Reader.fd stdin) dupped_stdin)
      in
      let stdout =
        Writer.create (create_fd ~similar_to:(Writer.fd stdout) dupped_stdout)
      in
      (stdin, stdout)
    in
    let make_sure_stdin_and_stdout_are_not_used () =
      (* After this, anyone attempting to read from stdin gets an empty result
         and anything written to stdout goes to stderr instead. *)
      let dev_null = Core.Unix.openfile ~mode:[O_RDONLY] "/dev/null" in
      Core.Unix.dup2 ~src:dev_null ~dst:Standard_fd.stdin;
      Core.Unix.dup2 ~src:Standard_fd.stderr ~dst:Standard_fd.stdout;
      Core.Unix.close dev_null
    in
    let res = make_a_copy_of_stdin_and_stdout () in
    make_sure_stdin_and_stdout_are_not_used ();
    res

  let main ?heartbeat_config ?log_not_previously_seen_version impls ~show_menu mode =
    if show_menu then
      let menu_sexp =
        [%sexp_of: (string * int) list] (Map.keys (menu impls))
      in
      write_sexp (Lazy.force Writer.stdout) menu_sexp;
      return `Success
    else
      let stdin, stdout = claim_stdin_and_stdout_for_exclusive_use () in
      match mode with
      | `Bin_prot ->
        begin
          match
            Rpc.Implementations.create
              ~on_unknown_rpc:`Raise
              ~implementations:
                (Versioned_rpc.Menu.add
                   (List.concat_map ~f:(implementations ?log_not_previously_seen_version)
                      impls))
          with
          | Error (`Duplicate_implementations descriptions) ->
            raise_s [%message "duplicate implementations"
                                (descriptions : Rpc.Description.t list)]
          | Ok implementations ->
            Rpc.Connection.server_with_close stdin stdout
              ?heartbeat_config
              ~implementations
              ~connection_state:(fun conn -> Bin_io conn)
              ~on_handshake_error:`Raise
            >>| fun () ->
            `Success
        end
      | `Sexp ->
        Reader.read_sexp stdin
        >>= function
        | `Eof -> failwith "unexpected EOF on stdin"
        | `Ok sexp ->
          let call = call_of_sexp sexp in
          match Map.find (menu impls) (call.rpc_name, call.version) with
          | None -> failwithf "unimplemented rpc: (%s, %d)" call.rpc_name call.version ()
          | Some impl ->
            match impl with
            | `Plain (module T) ->
              let query = T.query_of_sexp call.query in
              T.implementation Sexp query
              >>| fun response ->
              write_sexp stdout (T.sexp_of_response response);
              `Success
            | `Plain_conv (module T) ->
              let query = T.query_of_sexp call.query in
              T.implementation Sexp query
              >>| fun response ->
              write_sexp stdout (T.sexp_of_response response);
              `Success
            | `Pipe (module T) ->
              let query = T.query_of_sexp call.query in
              T.implementation Sexp query
              >>= function
              | Error e ->
                write_sexp stdout (T.sexp_of_error e);
                return `Failure
              | Ok pipe ->
                Pipe.iter pipe ~f:(fun r ->
                  write_sexp stdout (T.sexp_of_response r);
                  Deferred.unit)
                >>| fun () ->
                `Success

  let async_main status_deferred =
    upon status_deferred (fun status ->
      Shutdown.shutdown begin
        match status with
        | `Success -> 0
        | `Failure -> 1
      end);
    never_returns (Scheduler.go ())

  let menu_doc = " dump a sexp representation of the rpc menu"
  let sexp_doc = " speak sexp instead of bin-prot"

  module Expert = struct
    let param_exit_status ?heartbeat_config ?log_not_previously_seen_version () =
      let open Command.Let_syntax in
      [%map_open
        let show_menu = flag "-menu" no_arg ~doc:menu_doc
        and sexp = flag "-sexp" no_arg ~doc:sexp_doc
        in fun impls ->
          main ?heartbeat_config ?log_not_previously_seen_version impls ~show_menu
            (if sexp then `Sexp else `Bin_prot)
      ]

    let param ?heartbeat_config ?log_not_previously_seen_version () =
      Command.Param.map
        (param_exit_status ?heartbeat_config ?log_not_previously_seen_version ())
        ~f:(fun main rpcs ->
          (* If you want to detect success or failure and do something appropriate, you
             can just do that from your RPC implementation. But we still need
             [param_exit_status] separately because [create] below doesn't have access to
             the RPC implementations. *)
          main rpcs
          >>| function (`Success | `Failure) -> ())
  end

  let create ?heartbeat_config ?log_not_previously_seen_version ~summary impls =
    let open Command.Let_syntax in
    Command.basic ~summary
      [%map_open
        let main =
          Expert.param_exit_status ?heartbeat_config ?log_not_previously_seen_version ()
        in fun () ->
          async_main (main impls)
      ]
end

module Connection = struct
  type 'a with_connection_args
    = ?heartbeat_config:Rpc.Connection.Heartbeat_config.t
    -> ?propagate_stderr : bool        (* defaults to true *)
    -> ?env              : Process.env (* defaults to [`Extend []] *)
    -> ?process_create   : (prog:string -> args:string list -> ?env:Process.env -> unit -> Process.t Deferred.Or_error.t)
    -> prog              : string
    -> args              : string list
    -> 'a

  let transfer_stderr child_stderr =
    Reader.transfer child_stderr (Writer.pipe (Lazy.force Writer.stderr))
    >>= fun () ->
    Reader.close child_stderr

  let connect_gen
        ?(process_create = fun ~prog ~args ?env () -> Process.create ~prog ~args ?env ())
        ~propagate_stderr ~env ~prog ~args f =
    process_create ~prog ~args ~env ()
    >>=? fun process ->
    let stdin  = Process.stdin  process in
    let stdout = Process.stdout process in
    let stderr = Process.stderr process in
    don't_wait_for begin
      if propagate_stderr
      then transfer_stderr stderr
      else Reader.drain stderr
    end;
    (* This is mainly so that when a user closes the connection (which closes stdin and
       stdout) we will also close stderr. *)
    don't_wait_for begin
      Writer.close_finished stdin
      >>= fun () ->
      Reader.close_finished stdout
      >>= fun () ->
      Reader.close stderr
    end;
    let wait = Process.wait process in
    f ~stdin ~stdout ~wait
  ;;

  let with_close ?heartbeat_config ?(propagate_stderr=true) ?(env=`Extend []) ?process_create
        ~prog ~args dispatch_queries =
    connect_gen ?process_create ~propagate_stderr ~env ~prog ~args
      (fun ~stdin ~stdout ~wait ->
         let%bind result =
           Rpc.Connection.with_close
             stdout stdin
             ?heartbeat_config
             ~connection_state:(fun _ -> ())
             ~on_handshake_error:(`Call (fun exn -> return (Or_error.of_exn exn)))
             ~dispatch_queries
         in
         let%bind exit_or_signal = wait in
         ignore (exit_or_signal : Unix.Exit_or_signal.t);
         return result)
  ;;

  let create ?heartbeat_config ?(propagate_stderr = true) ?(env=`Extend []) ?process_create ~prog ~args () =
    connect_gen ?process_create ~propagate_stderr ~env ~prog ~args
      (fun ~stdin ~stdout ~wait ->
         don't_wait_for (Deferred.ignore (wait : Unix.Exit_or_signal.t Deferred.t));
         Rpc.Connection.create
           stdout stdin
           ?heartbeat_config
           ~connection_state:(fun _ -> ())
         >>| Or_error.of_exn_result)
  ;;
end
