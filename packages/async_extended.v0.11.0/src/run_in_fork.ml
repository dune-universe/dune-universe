open Core
open Async

type 'a ret =
  | Part of 'a
  | Err of Error.t
  | Done
  [@@deriving bin_io]
;;

let do_write writer bin_writer_t bigstring value  =
  let data_len = bin_writer_t.Bin_prot.Type_class.size value in
  let total_len = data_len + Bin_prot.Utils.size_header_length in
  if total_len > Bigstring.length !bigstring then begin
    bigstring := Bigstring.create total_len
  end;
  let len = Bigstring.write_bin_prot !bigstring bin_writer_t value in
  Bigstring.really_write writer !bigstring ~pos:0 ~len
;;

let in_the_child ~f bin_writer_t writer error_writer =
  let bigstring = ref (Bigstring.create 32) in
  let bin_writer_t = bin_writer_ret bin_writer_t in
  let shutdown code =
    do_write writer bin_writer_t bigstring Done;
    Core.Unix.close writer;
    if not (Core.Unix.File_descr.equal writer error_writer) then begin
      do_write error_writer bin_writer_t bigstring Done;
      Core.Unix.close error_writer;
    end;
    Core.Unix.exit_immediately code
  in
  try
    f ~write:(fun res -> do_write writer bin_writer_t bigstring (Part res));
    shutdown 0
  with exn ->
    do_write error_writer bin_writer_t bigstring (Err (Error.of_exn exn));
    shutdown 1
;;

let unexpected_eof_error = Error.of_string "Unexpected EOF in pipe from forked process"

module Multiple = struct
  let in_the_parent ?max_len bin_reader_t reader error_reader child_pid =
    let fd = Fd.create Fd.Kind.Fifo reader (Info.of_string "reader") in
    let async_reader = Reader.create fd in
    let error_fd = Fd.create Fd.Kind.Fifo error_reader (Info.of_string "error_reader") in
    let error_async_reader = Reader.create error_fd in
    let bin_reader_t = bin_reader_ret bin_reader_t in
    let read_result () =
      Reader.read_bin_prot async_reader bin_reader_t ?max_len
      >>| function
      | `Eof -> Err unexpected_eof_error
      | `Ok result -> result
    in
    let read_error =
      Reader.read_bin_prot error_async_reader bin_reader_t
      >>= function
      | `Eof -> Deferred.never ()
      | `Ok result -> return result
    in
    let finished () =
      Deferred.all_unit
        [ Reader.close async_reader
        ; Reader.close error_async_reader
        ; Deferred.ignore (Unix.waitpid child_pid)
        ]
      >>| fun () ->
      `Finished ()
    in
    Pipe.create_reader ~close_on_exception:true (fun writer ->
      let pipe_closed = Deferred.choice (Pipe.closed writer) (fun () -> Done) in
      Deferred.repeat_until_finished () (fun () ->
        Deferred.choose
          [ Deferred.choice (read_result ()) Fn.id
          ; Deferred.choice read_error Fn.id
          ; pipe_closed
          ]
        >>= fun result ->
        if Pipe.is_closed writer
        then finished ()
        else begin
          Pipe.write_without_pushback writer result;
          match result with
          | Done | Err _ -> finished ()
          | Part _       ->
            Pipe.pushback writer
            >>| fun () ->
            `Repeat ()
        end))
  ;;

  let run_in_fork ?max_len ~bin_t ~f () =
    let (reader, writer)             = Core.Unix.pipe () in
    let (error_reader, error_writer) = Core.Unix.pipe () in
    match Core.Unix.fork () with
    | `In_the_child ->
      Core.Unix.close reader;
      Core.Unix.close error_reader;
      in_the_child ~f bin_t.Bin_prot.Type_class.writer writer writer
    | `In_the_parent child_pid ->
      Core.Unix.close writer;
      Core.Unix.close error_writer;
      in_the_parent ?max_len bin_t.Bin_prot.Type_class.reader reader error_reader child_pid
  ;;
end

module Single = struct
  let run_in_fork ?max_len ~bin_t ~f () =
    let pipe = Multiple.run_in_fork ?max_len ~bin_t ~f:(fun ~write -> write (f ())) () in
    Pipe.read_exactly pipe ~num_values:2
    >>| fun result ->
    Pipe.close_read pipe;
    match result with
    | `Eof | `Fewer _ -> Error unexpected_eof_error
    | `Exactly q ->
      match Queue.to_list q with
      | [Part x; Done] -> Ok x
      | Err e :: _     -> Error e
      | _              -> Or_error.error_string "Unexpected return value from forked process"
  ;;
end

let run_in_fork = Single.run_in_fork
let run_in_fork_multiple = Multiple.run_in_fork
