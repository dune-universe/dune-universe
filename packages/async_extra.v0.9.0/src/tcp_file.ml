open Core
open Import

module Protocol = struct
  let version = 1

  module Open_file = struct
    module Mode = struct
      type t =
        | Read
        | Tail
      [@@deriving bin_io, sexp]
    end

    module Error = struct
      type t =
        | File_not_found of string
        | Unknown        of string
      [@@deriving bin_io, sexp]

      let to_string t = Sexp.to_string_hum (sexp_of_t t)
    end

    module Query = struct
      type t = Open of string * Mode.t [@@deriving bin_io, sexp]
    end

    module Message : sig
      type t =
        | String    of string
        | Bigstring of Bigstring.t
      [@@deriving bin_io, sexp_of]

      val length        : t -> int
      val to_string     : t -> string option
      val to_string_exn : t -> string
      val to_bigstring  : t -> Bigstring.t
    end = struct
      type t =
        | String    of string
        | Bigstring of Bigstring.t
      [@@deriving bin_io]

      let length t =
        match t with
        | String s     -> String.length s
        | Bigstring bs -> Bigstring.length bs

      let to_string_exn t =
        match t with
        | String s     -> s
        | Bigstring bs ->
          Bigstring.to_string bs ~pos:0 ~len:(Bigstring.length bs)

      let to_string t =
        try
          Some (to_string_exn t)
        with
        | _ -> None

      let to_bigstring t =
        match t with
        | String s     -> Bigstring.of_string s
        | Bigstring bs -> bs

      let sexp_of_t t : Sexp.t =
        match t with
        | String s    -> List [ Atom "String"; Atom s]
        | Bigstring _ -> Atom "Bigstring <opaque>"
    end

    module Response = struct
      type t = (Message.t, Error.t) Result.t [@@deriving bin_io]
    end

    let rpc ?client_pushes_back () =
      Rpc.Pipe_rpc.create
        ?client_pushes_back
        ~name:"open_file"
        ~version
        ~bin_query:Query.bin_t
        ~bin_response:Response.bin_t
        ~bin_error:Unit.bin_t
        ()
  end
end

let canonicalize filename =
  (* Remove multiple slashes in [filename]. *)
  (* It would be nice to use [realpath] for this, but I think it's problematic on
     occasion, since it seems to insist the file exists. *)
  let non_empty s = String.length s > 0 in
  let reform remainder = String.concat (List.filter remainder ~f:non_empty) ~sep:"/" in
  match String.split filename ~on:'/' with
  | ""::remainder -> "/" ^ (reform remainder)
  | remainder -> reform remainder

module Server = struct
  module File = struct

    type t =
      { filename                                        : string
      ; writer                                          : [ `Writer of File_writer.t
                                                          | `This_is_a_static_file
                                                          ]
      ; tail                                            : Protocol.Open_file.Message.t Tail.t
      ; line_ending                                     : [ `Dos | `Unix ]
      ; mutable num_lines_on_disk_after_flushing_writer : int
      ; mutable status                                  : [ `Open
                                                          | `Closing of unit Deferred.t
                                                          | `Closed
                                                          ]
      }
    [@@deriving sexp_of]
  end

  module Atomic_operations : sig
    val snapshot_state : File.t
      -> ([ `Read_this_many_lines_from_disk of int ]
          * [ `Then_read_from of Protocol.Open_file.Message.t Stream.t ])
           Deferred.t

    val write_message : File.t -> string -> File_writer.t -> unit
    val schedule_message : File.t -> Bigstring.t -> File_writer.t -> unit
  end = struct
    let snapshot_state (t : File.t) =
      (* begin critical section -- no Async context switch allowed *)
      let num_on_disk = t.num_lines_on_disk_after_flushing_writer in
      let lines_read_without_hitting_the_disk = Tail.collect t.tail in
      (* end critical section *)
      (match t.writer with
       | `Writer writer -> File_writer.flushed writer
       | `This_is_a_static_file -> (assert (Tail.is_closed t.tail); Deferred.unit))
      >>| fun () ->
      (* The file is divided into two sections: the first [num_on_disk] lines (read from
         disk) and a subsequent portion (read from the Tail).  The invariant is that
         these two portions form a contiguous portion of the file (i.e. they do not
         overlap and there is no gap between them). *)
      `Read_this_many_lines_from_disk num_on_disk,
      `Then_read_from lines_read_without_hitting_the_disk

    let line_ending_to_string = function
      | `Dos -> "\r\n"
      | `Unix -> "\n"

    let already_has_line_ending s ~line_ending ~get ~length =
      let line_ending_str = line_ending_to_string line_ending in
      let line_ending_length = String.length line_ending_str in
      let s_length = length s in
      if s_length < line_ending_length
      then false
      else
        let expect_line_ending_at = s_length - line_ending_length in
        List.for_all (List.range 0 line_ending_length)
          ~f:(fun x -> get s (expect_line_ending_at + x) = String.get line_ending_str x)

    let write_core (t : File.t) ~msg ~writer ~write_to_file ~length ~get ~protocol_msg =
      let line_ending = t.line_ending in
      (* begin critical section -- no Async context switch allowed *)
      t.num_lines_on_disk_after_flushing_writer <-
        t.num_lines_on_disk_after_flushing_writer + 1;
      write_to_file writer msg;
      if not (already_has_line_ending msg ~line_ending ~get ~length) then
        File_writer.write writer (line_ending_to_string line_ending);
      Tail.extend t.tail protocol_msg
    (* end critical section *)

    let write_message t msg writer =
      write_core t
        ~msg
        ~writer
        ~write_to_file:File_writer.write
        ~length:String.length
        ~get:String.get
        ~protocol_msg:(String msg)

    let schedule_message t msg writer =
      write_core t
        ~msg
        ~writer
        ~write_to_file:File_writer.schedule_bigstring
        ~length:Bigstring.length
        ~get:Bigstring.get
        ~protocol_msg:(Bigstring msg)
  end

  module State = struct
    module Serving_on = struct
      type t =
        [ `Not_yet_serving | `Serving_started | `Server of Tcp.Server.inet ]

      let sexp_of_t t : Sexp.t =
        match t with
        | `Not_yet_serving -> Atom "Not_yet_serving"
        | `Serving_started -> Atom "Serving_started"
        | `Server server   ->
          let port = Tcp.Server.listening_on server in
          List [ Atom "Port"; Int.sexp_of_t port ]

      let to_string t = Sexp.to_string (sexp_of_t t)
    end

    type t =
      { files              : File.t String.Table.t
      ; mutable serving_on : Serving_on.t
      }
    [@@deriving sexp_of]

    let global =
      { files      = String.Table.create ()
      ; serving_on = `Not_yet_serving
      }
  end

  let debug_snapshot () = State.sexp_of_t State.global

  let with_aborted input ~aborted =
    Deferred.choose [
      Deferred.choice aborted (fun () -> `Aborted);
      Deferred.choice input (fun v -> `Read v);
    ]

  let send_msg_to_client w msg = Pipe.write w (Ok msg)

  let tail stream w aborted =
    Deferred.create (fun ivar ->
      let stop_tailing = Ivar.fill ivar in
      let rec loop stream =
        with_aborted (Stream.next stream) ~aborted
        >>> function
        | `Aborted | `Read Nil -> stop_tailing ()
        | `Read (Cons (msg, rest)) -> send_msg_to_client w msg >>> fun () -> loop rest
      in
      loop stream)

  exception Unexpected_eof_when_reading_lines
    of string * [ `Wanted_to_read of int ] * [ `But_only_managed of int ]
  [@@deriving sexp]

  let read (t : File.t) w aborted =
    Atomic_operations.snapshot_state t
    >>= function (`Read_this_many_lines_from_disk num_from_disk, `Then_read_from rest) ->
      Reader.with_file t.filename ~f:(fun r ->
        Deferred.create (fun ivar ->
          let stop_reading = Ivar.fill ivar in
          let rec read_lines ~current_line =
            if current_line <= num_from_disk
            then
              with_aborted (Reader.read_line r) ~aborted
              >>> function
              | `Read `Ok msg ->
                send_msg_to_client w (Protocol.Open_file.Message.String msg) >>> fun () ->
                read_lines ~current_line:(current_line + 1)
              | `Aborted      -> stop_reading ()
              | `Read `Eof    ->
                raise (Unexpected_eof_when_reading_lines
                         (t.filename,
                          `Wanted_to_read num_from_disk,
                          `But_only_managed (current_line - 1)))
            else (
              don't_wait_for (Reader.close r);
              tail rest w aborted >>> stop_reading
            )
          in
          read_lines ~current_line:1))

  let tail (t : File.t) w aborted = tail (Tail.collect t.tail) w aborted

  let handle_open_file state query =
    let module Open_file = Protocol.Open_file in
    let Open_file.Query.Open (filename, mode) = query in
    let pipe_r, pipe_w = Pipe.create () in
    Monitor.try_with (fun () ->
      let dispatch filename f =
        match String.Table.find state.State.files (canonicalize filename) with
        | None   ->
          Pipe.write pipe_w
            (Error (Open_file.Error.File_not_found filename))
          >>| fun () ->
          Pipe.close pipe_w
        | Some file -> f file pipe_w (Pipe.closed pipe_w)
      in
      match mode with
      | Read -> dispatch filename read
      | Tail -> dispatch filename tail)
    >>> (function
      | Ok ()   -> Pipe.close pipe_w
      | Error e ->
        if not (Pipe.is_closed pipe_w) then begin
          don't_wait_for (Pipe.write pipe_w (Error (Unknown (Exn.to_string e))));
          Pipe.close pipe_w
        end);
    Deferred.return (Ok pipe_r)

  let implementations =
    [ Rpc.Pipe_rpc.implement
        (* The client pushes back parameter has no effect on the server side of Async.Rpc at
           the time of this implementation. Exposing it would be misleading. *)
        (Protocol.Open_file.rpc ())
        handle_open_file
    ]

  let serve ~auth where_to_listen =
    match State.global.serving_on with
    | `Serving_started
    | `Server _ ->
      failwithf !"Tcp_file.Server.serve called twice.  \
                  (current state: %{State.Serving_on})"
        State.global.serving_on ()
    | `Not_yet_serving ->
      let implementations =
        Rpc.Implementations.create ~implementations ~on_unknown_rpc:`Close_connection
        |> function
        | Ok s -> State.global.serving_on <- `Serving_started; s
        | Error (`Duplicate_implementations _) -> assert false
      in
      Rpc.Connection.serve ~auth ~implementations ~where_to_listen ()
        ~initial_connection_state:(fun _ _ -> State.global)
      >>| fun server ->
      State.global.serving_on <- `Server server;
      server
  ;;

  exception File_is_already_open_in_tcp_file of string [@@deriving sexp]

  let count_lines filename =
    Sys.file_exists filename
    >>= function
    | `No      -> Deferred.return 0
    | `Unknown -> failwithf "unable to open file: %s" filename ()
    | `Yes ->
      (* There is no strong case for using [~exclusive:true] here, since locks are
         advisory, but it expresses something in the code that we want to be true, and
         shouldn't hurt. *)
      Reader.with_file ~exclusive:true filename
        ~f:(fun r -> Pipe.drain_and_count (Reader.lines r))
  ;;

  let open_file
        ?(append = false)
        ?(dos_format = false)
        filename =
    let filename = canonicalize filename in
    match String.Table.find State.global.files filename with
    | Some _ -> raise (File_is_already_open_in_tcp_file filename)
    | None   ->
      let num_lines_already_on_disk =
        if append
        then count_lines filename
        else return 0
      in
      num_lines_already_on_disk
      >>= fun num_lines_already_on_disk ->
      File_writer.create filename ~append:append
      >>| fun writer ->
      let file =
        { File.
          filename
        ; writer                                  = `Writer writer
        ; tail                                    = Tail.create ()
        ; line_ending                             = if dos_format then `Dos else `Unix
        ; num_lines_on_disk_after_flushing_writer = num_lines_already_on_disk
        ; status                                  = `Open;
        }
      in
      String.Table.set State.global.files ~key:filename ~data:file;
      file
  ;;

  let stop_serving_internal (t : File.t) =
    String.Table.remove State.global.files t.filename
  ;;

  let stop_serving = stop_serving_internal

  let close ?(stop_serving = true) (t : File.t) =
    match t.status with
    | `Closed         -> Deferred.unit
    | `Closing closed -> closed
    | `Open           ->
      let close_notification = Ivar.create () in
      t.status <- `Closing (Ivar.read close_notification);
      if stop_serving then stop_serving_internal t;
      Tail.close_if_open t.tail;
      let closed =
        match t.writer with
        | `Writer writer         -> File_writer.close writer
        | `This_is_a_static_file -> Deferred.unit
      in
      upon closed (fun () ->
        Ivar.fill close_notification ();
        t.status <- `Closed);
      Ivar.read close_notification;
  ;;

  exception Attempt_to_flush_static_tcp_file of string [@@deriving sexp]

  let flushed (t : File.t) =
    match t.writer with
    | `Writer writer -> File_writer.flushed writer
    | `This_is_a_static_file -> raise (Attempt_to_flush_static_tcp_file t.filename)
  ;;

  exception Attempt_to_write_message_to_closed_tcp_file of string [@@deriving sexp]
  exception Attempt_to_write_message_to_static_tcp_file of string [@@deriving sexp]

  let gen_message (t : File.t) f =
    match t.writer with
    | `This_is_a_static_file ->
      raise (Attempt_to_write_message_to_static_tcp_file t.filename)
    | `Writer writer ->
      match t.status with
      | `Closing _ | `Closed ->
        raise (Attempt_to_write_message_to_closed_tcp_file t.filename);
      | `Open ->
        f writer
  ;;

  let write_message t msg =
    gen_message t (fun writer -> Atomic_operations.write_message t msg writer)
  ;;

  let schedule_message t msg =
    gen_message t (fun writer -> Atomic_operations.schedule_message t msg writer)
  ;;

  let write_sexp =
    (* We use strings for Sexps whose string representations can fit on the minor heap and
       Bigstring.t's for those that can't. *)
    let max_num_words_allocatable_on_minor_heap = 256 in
    let bytes_per_word = 8 in
    let buf = Bigbuffer.create 1024 in
    fun t sexp ->
      Bigbuffer.clear buf;
      Sexp.to_buffer_gen sexp ~buf ~add_char:Bigbuffer.add_char
        ~add_string:Bigbuffer.add_string;
      let buf_size_in_words =
        (* This is the same calculation that the runtime uses.

           Remember that space is left in the Caml value for a NULL terminator.
           So if the word size is 8 bytes and the string is 8 bytes long, we
           need two words, for example. *)
        (Bigbuffer.length buf + bytes_per_word) / bytes_per_word
      in
      if buf_size_in_words <= max_num_words_allocatable_on_minor_heap
      then write_message t (Bigbuffer.contents buf)
      else schedule_message t (Bigbuffer.big_contents buf);
  ;;

  let with_file ?append filename ~f =
    open_file ?append filename >>= fun t ->
    Monitor.try_with (fun () -> f t) >>= fun res ->
    close t >>| fun () ->
    Result.ok_exn res
  ;;

  let serve_existing_static_file filename =
    let filename = canonicalize filename in
    match String.Table.find State.global.files filename with
    | None   ->
      count_lines filename >>| fun num_lines_on_disk_after_flushing_writer ->
      let tail = Tail.create () in
      Tail.close_if_open tail;
      let file =
        { File.
          filename
        ; writer                                  = `This_is_a_static_file
        ; line_ending                             = `Unix  (* Arbitrary setting: will never be used. *)
        ; tail
        ; num_lines_on_disk_after_flushing_writer
        ; status                                  = `Closed
        }
      in
      String.Table.set State.global.files ~key:filename ~data:file
    | Some _ -> raise (File_is_already_open_in_tcp_file filename)
  ;;

  let writer_monitor (t : File.t) =
    match t.writer with
    | `Writer writer         -> Ok (File_writer.monitor writer)
    | `This_is_a_static_file -> Error `This_is_a_static_file
  ;;
end

module Client = struct
  type t = Rpc.Connection.t

  module Error    = Protocol.Open_file.Error
  module Message  = Protocol.Open_file.Message
  module Response = Protocol.Open_file.Response

  let connect ~host ~port = Rpc.Connection.client ~host ~port ()
  let disconnect t = Rpc.Connection.close t

  let read ?client_pushes_back t filename =
    let rpc = Protocol.Open_file.rpc ?client_pushes_back () in
    let filename = canonicalize filename in
    Rpc.Pipe_rpc.dispatch_exn rpc t (Open (filename, Read))
    >>| fun (pipe_r, md) ->
    Pipe.closed pipe_r >>> (fun () ->
      Rpc.Pipe_rpc.abort rpc t (Rpc.Pipe_rpc.Metadata.id md));
    pipe_r

  let tail ?client_pushes_back t filename =
    let filename = canonicalize filename in
    let rpc = Protocol.Open_file.rpc ?client_pushes_back () in
    Rpc.Pipe_rpc.dispatch_exn rpc t (Open (filename, Tail))
    >>| fun (pipe_r, md) ->
    Pipe.closed pipe_r >>> (fun () ->
      Rpc.Pipe_rpc.abort rpc t (Rpc.Pipe_rpc.Metadata.id md));
    pipe_r
end
