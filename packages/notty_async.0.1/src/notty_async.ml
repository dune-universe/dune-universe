open! Base
open! Async

include struct
  open Notty
  module Unescape = Unescape
  module Tmachine = Tmachine
end

module Winch_listener = struct
  let waiting = ref []

  external winch_number : unit -> int = "caml_notty_winch_number" [@@noalloc]
  let sigwinch = Async.Signal.of_caml_int (winch_number ())

  let setup_winch = lazy (
    Signal.handle [sigwinch] ~f:(fun (_:Signal.t) ->
        List.iter !waiting ~f:(fun i -> Ivar.fill i ());
        waiting := []))

  let winch () =
    force setup_winch;
    let i = Ivar.create () in
    waiting := i :: !waiting;
    Ivar.read i
end

module Term = struct

  let bsize = 1024

  (* CR yminsky: Maybe turn this into just ac all that just reads the next
     input, rather than one that creates a pipe.  *)
  (* Call [f] function repeatedly as input is received from the
     stream. *)
  let input_pipe ~nosig reader =
    let `Revert revert =
      let fd = Unix.Fd.file_descr_exn (Reader.fd reader) in
      Notty_unix.Private.setup_tcattr ~nosig fd
    in
    let flt  = Notty.Unescape.create () in
    let ibuf = Bytes.create bsize in
    let (r,w) = Pipe.create () in
    let rec loop () =
      match Unescape.next flt with
      | #Unescape.event as r ->
        (* As long as there are events to read without blocking, dump
           them all into the pipe. *)
        if Pipe.is_closed w then return ()
        else (Pipe.write_without_pushback w r; loop ())
      | `End   -> return ()
      | `Await ->
        (* Don't bother issuing a new read until the pipe has space to
           write *)
        let%bind () = Pipe.pushback w in
        match%bind Reader.read reader ibuf with
        | `Eof -> return ()
        | (`Ok n)  -> Unescape.input flt ibuf 0 n; loop ()
    in
    (* Some error handling to make sure that we call revert if the pipe fails *)
    let monitor = Monitor.create ~here:[%here] ~name:"Notty input pipe" () in
    don't_wait_for (Deferred.ignore (Monitor.get_next_error monitor) >>| revert);
    don't_wait_for (Scheduler.within' ~monitor loop);
    don't_wait_for (Pipe.closed r >>| revert);
    r

  type t =
    { writer   : Writer.t
    ; tmachine : Tmachine.t
    ; buf      : Buffer.t
    ; fds      : Fd.t * Fd.t
    ; events   : [ Unescape.event | `Resize of (int * int) ] Pipe.Reader.t
    ; stop     : (unit -> unit)
    }

  let write t =
    Buffer.clear t.buf;
    Tmachine.output t.tmachine t.buf;
    Writer.write t.writer (Buffer.contents t.buf);
    Writer.flushed t.writer

  let refresh t      = Tmachine.refresh t.tmachine; write t
  let image t image  = Tmachine.image t.tmachine image; write t
  let cursor t curs  = Tmachine.cursor t.tmachine curs; write t
  let set_size t dim = Tmachine.set_size t.tmachine dim
  let size t         = Tmachine.size t.tmachine

  let release t =
    if Tmachine.release t.tmachine then (t.stop (); write t) else return ()

  let resize_pipe_and_update_tmachine tmachine writer  =
    let (r,w) = Pipe.create () in
    don't_wait_for (
      match%bind Unix.isatty (Writer.fd writer) with
      | false -> Pipe.close w; return ()
      | true ->
        let rec loop () =
          let%bind () = Winch_listener.winch () in
          match Fd.with_file_descr (Writer.fd writer) Notty_unix.winsize with
          | `Already_closed | `Error _ -> return ()
          | `Ok size ->
            match size with
            | None ->
              (* Note 100% clear that this is the right behavior,
                 since it's not clear why one would receive None from
                 winsize at all.  In any case, causing further resizes
                 should cause an app to recover if there's a temporary
                 inability to read the size. *)
              loop ()
            | Some size ->
              if Pipe.is_closed w then return ()
              else (
                Tmachine.set_size tmachine size;
                let%bind () = Pipe.write w (`Resize size) in
                loop ())
        in
        let%map () = loop () in
        Pipe.close w);
    r

  let create
      ?(dispose=true)
      ?(nosig=true)
      ?(mouse=true)
      ?(bpaste=true)
      ?(reader=(force Reader.stdin))
      ?(writer=(force Writer.stdout))
      ()
    =
    let (cap,size) =
      Fd.with_file_descr_exn (Writer.fd writer)
        (fun fd ->
           (Notty_unix.Private.cap_for_fd fd,
            Notty_unix.winsize fd))
    in
    let tmachine = Tmachine.create ~mouse ~bpaste cap in
    let input_pipe = input_pipe ~nosig reader in
    let resize_pipe = resize_pipe_and_update_tmachine tmachine writer in
    let events = Pipe.interleave [input_pipe; resize_pipe] in
    let stop () = Pipe.close_read events in
    let buf = Buffer.create 4096 in
    let fds = (Reader.fd reader, Writer.fd writer) in
    let t = { tmachine; writer; events; stop; buf; fds } in
    Option.iter size ~f:(set_size t);
    if dispose then Shutdown.at_shutdown (fun () -> release t);
    don't_wait_for (
      let%bind () = Pipe.closed events in
      release t);
    let%map () = write t in
    t

  let events t = t.events
end

include Notty_unix.Private.Gen_output (struct
  type fd = Writer.t lazy_t and k = unit Deferred.t

  let def =
    Writer.stdout

  let to_fd w =
    (* CR yminsky: Here, we're defeating the purpose of Async's idioms
       for avoiding operating on closed FDs. To do something better,
       we'd need to adjust the API of Notty's Gen_output functor, or
       replicate it. *)
    match Fd.with_file_descr (Writer.fd (force w)) Fn.id with
    | `Already_closed | `Error _ -> raise_s [%message "Couldn't obtain FD"]
    | `Ok x -> x

  let write (lazy w) buf =
    let bytes = Buffer.contents_bytes buf in
    Writer.write_bytes w bytes ~pos:0 ~len:(Bytes.length bytes);
    Writer.flushed w
end)
