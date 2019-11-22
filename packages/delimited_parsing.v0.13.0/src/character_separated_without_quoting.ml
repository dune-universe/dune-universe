open Core
open Async
open Shared
module Row = Delimited_kernel.Read.Row
module Header = Delimited_kernel.Read.Header

let of_reader
      ?(strip = false)
      ?(skip_lines = 0)
      ?(on_parse_error = `Raise)
      ~header
      ?(quote = '\\')
      ~sep
      reader
  =
  let module Table = String.Table in
  assert (Char.O.(quote <> sep));
  let lineno = ref 1 in
  let pipe_r, pipe_w = Pipe.create () in
  let buffer = Bytes.create buffer_size in
  let field = Buffer.create 1 in
  let quoted = ref false in
  let current_row = Queue.create () in
  let row_queue = Queue.create () in
  let emit_field = unstage (make_emit_field ~strip current_row field) in
  let `on_eof on_eof, emit_row = make_emit_row current_row row_queue header ~lineno in
  let flush_rows () = Pipe.transfer_in pipe_w ~from:row_queue in
  let prev_was_cr = ref false in
  let emit_pending_cr () =
    if !prev_was_cr
    then (
      Buffer.add_char field '\r';
      prev_was_cr := false)
  in
  let add_char c =
    (* delay adding '\r' characters until we know that the next character is
       not '\n' *)
    emit_pending_cr ();
    if Char.equal c '\r' then prev_was_cr := true else Buffer.add_char field c
  in
  let close () =
    don't_wait_for (flush_rows ());
    don't_wait_for (Reader.close reader);
    Pipe.close pipe_w
  in
  let rec loop () =
    Reader.read reader buffer
    >>> function
    | `Eof ->
      if Queue.length current_row <> 0
      then (
        emit_field ();
        emit_row ());
      on_eof ();
      close ()
    | `Ok n ->
      let res =
        Result.try_with (fun () ->
          for i = 0 to n - 1 do
            let c = Bytes.get buffer i in
            if Char.equal c '\n'
            then (
              prev_was_cr := false;
              if !quoted
              then (
                close ();
                failwithf
                  "escape character found at the end of a line (lineno=%d)"
                  !lineno
                  ())
              else (
                emit_field ();
                emit_row ()))
            else if !quoted
            then (
              quoted := false;
              add_char c)
            else if Char.equal c sep
            then (
              emit_pending_cr ();
              emit_field ())
            else if Char.equal c quote
            then (
              emit_pending_cr ();
              quoted := true)
            else add_char c
          done)
      in
      flush_rows ()
      >>> fun () ->
      (match res with
       | Ok () -> loop ()
       | Error e ->
         (match on_parse_error with
          | `Raise -> raise e
          | `Handle f ->
            emit_field ();
            (match f current_row e with
             | `Continue -> loop ()
             | `Finish -> close ())))
  in
  upon (drop_lines reader skip_lines) loop;
  pipe_r
;;

let create_reader ?strip ?skip_lines ?on_parse_error ~header ?quote ~sep filename =
  let%map r = Reader.open_file filename in
  of_reader ?strip ?skip_lines ?on_parse_error ~header ?quote ~sep r
;;
