open! Core
open! Async

open Shared

let of_reader
    ?(strip=false)
    ?(skip_lines=0)
    ?(on_parse_error=`Raise)
    ~header
    ?(quote='\\')
    ~sep
    reader =
  let module Table = String.Table in
  assert (quote <> sep);
  let lineno        = ref 1 in
  let pipe_r,pipe_w = Pipe.create () in
  let buffer        = Bytes.create buffer_size in
  let field         = Buffer.create 1 in
  let quoted        = ref false in
  let current_row   = Queue.create () in
  let row_queue     = Queue.create () in
  let emit_field    = unstage (make_emit_field ~strip current_row field) in
  let `on_eof on_eof, emit_row = make_emit_row current_row row_queue header ~lineno in
  let flush_rows () =
    Pipe.transfer_in pipe_w ~from:row_queue
  in
  let prev_was_cr   = ref false in
  let emit_pending_cr () =
    if !prev_was_cr then begin
      Buffer.add_char field '\r';
      prev_was_cr := false
    end
  in
  let add_char c =
    (* delay adding '\r' characters until we know that the next character is
       not '\n' *)
    emit_pending_cr ();
    if c = '\r' then
      prev_was_cr := true
    else
      Buffer.add_char field c
  in
  let close () =
    don't_wait_for (flush_rows ());
    don't_wait_for (Reader.close reader);
    Pipe.close pipe_w
  in
  let rec loop () =
    Reader.read reader buffer >>> function
    | `Eof ->
      if Queue.length current_row <> 0 then begin
        emit_field ();
        emit_row ();
      end;
      on_eof ();
      close ()
    | `Ok n ->
      let res =
        Result.try_with (fun () ->
          for i = 0 to n - 1 do
            let c = Bytes.get buffer i in
            if c = '\n' then begin
              prev_was_cr := false;
              if !quoted then begin
                close ();
                failwithf "escape character found at the end of a line (lineno=%d)"
                  (!lineno) ()
              end else begin
                emit_field ();
                emit_row ()
              end
            end else if !quoted then begin
              quoted := false;
              add_char c
            end else if c = sep then begin emit_pending_cr (); emit_field () end
            else if c = quote then begin emit_pending_cr (); quoted := true end
            else add_char c
          done)
      in
      flush_rows ()
      >>> fun () ->
      match res with
      | Ok ()   -> loop ()
      | Error e ->
        match on_parse_error with
        | `Raise    -> raise e
        | `Handle f ->
          emit_field ();
          match f current_row e with
          | `Continue -> loop ()
          | `Finish   -> close ()
  in
  upon (drop_lines reader skip_lines) loop;
  pipe_r
;;

let create_reader ?strip ?skip_lines ?on_parse_error ~header ?quote ~sep filename =
  Reader.open_file filename >>| fun r ->
  of_reader ?strip ?skip_lines ?on_parse_error ~header ?quote ~sep r
;;
