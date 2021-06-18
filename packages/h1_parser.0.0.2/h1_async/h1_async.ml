open Core
open Async

type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

let set_nonblock fd = Fd.with_file_descr_exn ~nonblocking:true fd ignore

module Read = struct
  type t = { fd : Fd.t; buf : Bytebuffer.t; decoder : H1.Decoder.decoder }

  let create fd size =
    set_nonblock fd;
    let buf = Bytebuffer.create size in
    { fd; buf; decoder = H1.Decoder.decoder () }

  let fill_buf t =
    let view = Bytebuffer.fill t.buf in
    let syscall_result =
      Bigstring_unix.read_assume_fd_is_nonblocking (Fd.file_descr_exn t.fd)
        view.Bytebuffer.View.buffer ~pos:view.pos ~len:view.len
    in
    if Unix.Syscall_result.Int.is_ok syscall_result then
      let count = Unix.Syscall_result.Int.ok_exn syscall_result in
      if count = 0 then `Eof
      else (
        view.continue count;
        `Ok)
    else
      match Unix.Syscall_result.Int.error_exn syscall_result with
      | EWOULDBLOCK | EAGAIN | EINTR -> `Poll_again
      | EPIPE | ECONNRESET | EHOSTUNREACH | ENETDOWN | ENETRESET | ENETUNREACH
      | ETIMEDOUT ->
          `Eof
      | err -> raise (Unix.Unix_error (err, "", ""))

  let rec refill t fn =
    match fill_buf t with
    | `Ok -> fn `Ok
    | `Eof -> fn `Eof
    | `Poll_again -> (
        Fd.ready_to t.fd `Read >>> function
        | `Bad_fd | `Closed ->
            raise_s
              [%message
                "H1_async_unix.write_all_pending: Bad file descriptor"
                  ~fd:(t.fd : Fd.t)]
        | `Ready -> refill t fn)

  let next_event t =
    match H1.Decoder.decode t.decoder with
    | `Need_data as res ->
        let consumed = H1.Decoder.consumed t.decoder in
        Bytebuffer.drop t.buf consumed;
        res
    | res -> res
end

module Write = struct
  type t = { fd : Fd.t; buf : Bytebuffer.t }

  let create fd size =
    set_nonblock fd;
    let buf = Bytebuffer.create size in
    { fd; buf }

  let write t =
    let consume = Bytebuffer.consume t.buf in
    match
      Bigstring_unix.write_assume_fd_is_nonblocking (Fd.file_descr_exn t.fd)
        consume.Bytebuffer.View.buffer ~pos:consume.pos ~len:consume.len
    with
    | count ->
        consume.continue count;
        `Ok
    | exception Unix.Unix_error ((EWOULDBLOCK | EAGAIN | EINTR), _, _) -> `Ok
    | exception
        Unix.Unix_error
          ( ( EPIPE | ECONNRESET | EHOSTUNREACH | ENETDOWN | ENETRESET
            | ENETUNREACH | ETIMEDOUT ),
            _,
            _ ) ->
        `Eof
    | exception exn -> raise exn

  let shutdown t = Fd.close t.fd

  let rec write_all_pending t ~f =
    match write t with
    | `Eof -> shutdown t >>> f
    | `Ok -> (
        if Bytebuffer.length t.buf = 0 then f ()
        else
          Fd.ready_to t.fd `Write >>> function
          | `Ready -> write_all_pending t ~f
          | `Bad_fd | `Closed ->
              raise_s
                [%message
                  "H1_async_unix.write_all_pending: Bad file descriptor"
                    ~fd:(t.fd : Fd.t)])
end

type conn = { read : Read.t; write : Write.t }

let create fd ~read_buffer_size ~write_buffer_size =
  let read = Read.create fd read_buffer_size in
  let write = Write.create fd write_buffer_size in
  { read; write }

let write_body t msg =
  match msg with
  | `Bigstring b -> Bytebuffer.add_bigstring t.write.Write.buf b
  | `String s -> Bytebuffer.add_string t.write.Write.buf s

type body_stream = unit -> string option Deferred.t

let make_body_stream t =
  let closed = ref false in
  let rec fn () =
    if !closed then return None
    else
      match Read.next_event t.read with
      | `Need_data ->
          Deferred.create (fun ivar ->
              Read.refill t.read (function
                | `Eof -> Ivar.fill_if_empty ivar ()
                | `Ok ->
                    let view = Bytebuffer.consume t.read.buf in
                    H1.Decoder.src t.read.decoder view.Bytebuffer.View.buffer
                      ~pos:view.pos ~len:view.len;
                    Ivar.fill_if_empty ivar ()))
          >>= fn
      | `Data s -> return (Some s)
      | `Error msg -> failwith msg
      | `Request _ -> failwith "Unexpected payload while parsing body"
      | `Request_complete ->
          closed := true;
          H1.Decoder.next_cycle t.read.decoder;
          return None
  in
  fn

let rec iter_body t ~f =
  match%bind t () with
  | None -> return ()
  | Some x ->
      let%bind () = f x in
      iter_body t ~f

let rec iter_body' t ~f =
  match%bind t () with
  | None -> return ()
  | Some x ->
      f x;
      iter_body' t ~f

let rec drain fn =
  match%bind fn () with None -> return () | Some _ -> drain fn

type service =
  Cohttp.Request.t * body_stream ->
  (Cohttp.Response.t * [ `Bigstring of bigstring | `String of string ])
  Deferred.t

let run t service =
  let close = Ivar.create () in
  let monitor = Monitor.create ~here:[%here] ~name:"H1_async.run_server" () in
  let rec aux () =
    match Read.next_event t.read with
    | `Need_data ->
        Read.refill t.read (function
          | `Eof -> Ivar.fill_if_empty close ()
          | `Ok ->
              let view = Bytebuffer.consume t.read.buf in
              H1.Decoder.src t.read.decoder view.Bytebuffer.View.buffer
                ~pos:view.pos ~len:view.len;
              aux ())
    | `Error msg -> failwith msg
    | `Request_complete ->
        H1.Decoder.next_cycle t.read.decoder;
        aux ()
    | `Request req ->
        let body_stream = make_body_stream t in
        service (req, body_stream) >>> fun (response, body) ->
        H1.serialize_response t.write.Write.buf response;
        write_body t body;
        Write.write_all_pending t.write ~f:(fun () ->
            drain body_stream >>> fun () ->
            H1.Decoder.next_cycle t.read.decoder;
            aux ())
    | `Data _ -> assert false
  in
  Scheduler.within ~monitor aux;
  Monitor.detach_and_iter_errors monitor ~f:(fun exn ->
      Log.Global.error "%s" (Exn.to_string exn);
      Ivar.fill_if_empty close ());
  Ivar.read close
