open Core
open Async
open Shared

module Csv_writer = Core_extended.Csv_writer

module State = struct
  type t =
    | StartField
    | InUnquotedField
    | InQuotedField
    | InQuotedFieldAfterQuote
end
open State

let default_separator = ','

exception Bad_csv_formatting = Csv.Bad_csv_formatting

(* Returns a function that takes values of the type:

   [ `Data of (string * int) | `Eof ]

   where the [int] argument to `Data indicates the prefix of the string that contains
   valid data.  This is useful when passing buffers filled by a Reader. *)
let create_chunk_processor
      ?(strip=false)
      ?sep:(separator=default_separator)
      ~header
      () =
  let lineno        = ref 1 in
  let state         = ref StartField in
  let field         = Buffer.create 1 in
  let current_row   = Queue.create () in
  let row_queue     = Queue.create () in
  let emit_field    = unstage (make_emit_field ~strip current_row field) in
  let `on_eof on_eof, emit_row =
    make_emit_row current_row row_queue header ~lineno
  in
  let flush_rows () =
    let data = Queue.to_list row_queue in
    Queue.clear row_queue;
    data
  in
  stage (function
    | `Eof ->
      begin
        match !state with
        | StartField ->
          if Queue.length current_row <> 0 then begin
            emit_field ();
            emit_row ()
          end;
        | InUnquotedField
        | InQuotedFieldAfterQuote ->
          emit_field ();
          emit_row ()
        | InQuotedField ->
          raise (Bad_csv_formatting (Queue.to_list current_row, Buffer.contents field))
      end;
      let result = flush_rows () in
      on_eof ();
      result
    | `Data (buffer, n) ->
      for i = 0 to n - 1 do
        let c = buffer.[i] in
        if c <> '\r' then
          match !state with
          | StartField ->
            if      c = '\"'      then state := InQuotedField
            else if c = separator then emit_field ()
            else if c = '\n'      then (emit_field (); emit_row ())
            else begin
              Buffer.add_char field c;
              state := InUnquotedField
            end
          | InUnquotedField ->
            begin
              if c = separator then
                (emit_field (); state := StartField)
              else if c = '\n' then (
                emit_field ();
                emit_row ();
                state := StartField)
              else Buffer.add_char field c
            end
          | InQuotedField ->
            if c = '\"' then
              state := InQuotedFieldAfterQuote
            else
              Buffer.add_char field c
          | InQuotedFieldAfterQuote ->
            if c = '\"' then ( (* doubled quote *)
              Buffer.add_char field c;
              state := InQuotedField)
            else if c = '0' then (
              Buffer.add_char field '\000';
              state := InQuotedField)
            else if c = separator then (
              emit_field ();
              state := StartField)
            else if c = '\n' then (
              emit_field ();
              emit_row ();
              state := StartField)
            else if Char.is_whitespace c then ()
            else
              failwithf "InQuotedFieldAfterQuote looking at '%c' (lineno=%d)"
                c (!lineno) ()
      done;
      flush_rows ())
;;

let create_manual
      ?strip
      ?sep
      ~header
      () =
  let process = unstage (create_chunk_processor ?strip ~header ?sep ()) in
  stage (function
    | `Eof    -> process `Eof
    | `Data s -> process (`Data (s, String.length s)))
;;

let parse_string
      ?strip
      ?sep
      ~header
      s =
  let process = unstage (create_chunk_processor ?strip ~header ?sep ()) in
  List.concat_map [`Data (s, String.length s); `Eof] ~f:process
;;

let of_reader
      ?strip
      ?(skip_lines=0)
      ?sep
      ~header
      reader =
  let pipe_r,pipe_w = Pipe.create () in
  let buffer        = Bytes.create buffer_size in
  let close () =
    Pipe.close pipe_w;
    don't_wait_for (Reader.close reader)
  in
  upon (Pipe.closed pipe_w) (fun () -> close ());
  let process = unstage (create_chunk_processor ?strip ~header ?sep ()) in
  let update s =
    if not (Pipe.is_closed pipe_w)
    then List.iter (process s) ~f:(Pipe.write_without_pushback pipe_w);
    Pipe.pushback pipe_w
  in
  let rec loop () =
    Monitor.try_with (fun () -> Reader.read reader buffer)
    >>= function
    | Error exn ->
      (* Reader.read throws an exception if the reader is closed.
         If the pipe has already closed, then we shouldn't care about
         these errors. *)
      if Pipe.is_closed pipe_w
      then Deferred.unit
      else begin
        close ();
        raise exn;
      end
    | Ok `Eof ->
      update `Eof
      >>| fun () ->
      close ()
    | Ok (`Ok n) ->
      update (`Data (Bytes.to_string buffer, n))
      >>= fun () ->
      loop ()
  in
  upon (drop_lines reader skip_lines) (fun () -> don't_wait_for (loop ()));
  pipe_r
;;

let create_reader ?strip ?skip_lines ?sep ~header filename =
  Reader.open_file filename >>| fun r ->
  of_reader ?strip ?skip_lines ~header ?sep r
;;

let write_field w field = Writer.write w (Csv_writer.maybe_escape_field field)

let rec write_line ~sep ~line_break w line =
  match line with
  | [] -> Writer.write w line_break
  | [field] ->
    write_field w field;
    write_line ~sep ~line_break w []
  | field :: rest ->
    write_field w field;
    Writer.write_char w sep;
    write_line ~sep ~line_break w rest
;;

let of_writer ?(sep=',') ?(line_breaks = `Windows) writer =
  let line_break =
    match line_breaks with
    | `Unix    -> "\n"
    | `Windows -> "\r\n"
  in
  let pipe_r, pipe_w = Pipe.create () in
  don't_wait_for (Writer.transfer writer pipe_r (write_line ~sep ~line_break writer));
  upon (Pipe.closed pipe_w) (fun () -> don't_wait_for (Writer.close writer));
  pipe_w
;;

let create_writer ?sep ?line_breaks filename =
  Writer.open_file filename
  >>| fun w ->
  of_writer ?sep ?line_breaks w
;;

let%test_unit "parse_string no headers" =
  let rows = parse_string ~sep:'|' ~header:`No "alpha|beta" in
  [%test_result: string list list] ~expect:[["alpha"; "beta"]]
    (List.map rows ~f:Row.to_list)
;;

let%test_unit "parse_string headers" =
  let rows = parse_string ~sep:'|' ~header:`Yes "foo|bar\nalpha|beta" in
  match rows with
  | [ row ] ->
    [%test_result: string option] ~expect:(Some "alpha") (Row.get row "foo");
    [%test_result: string option] ~expect:(Some "beta") (Row.get row "bar")
  | _ ->
    failwithf "unexpected number of rows %d, expected 1" (List.length rows) ()
;;

let%expect_test "required header is also required for empty files" =
  Expect_test_helpers.show_raise (fun () ->
    parse_string ~sep:'|' ~header:`Yes "");
  [%expect {| (raised (Failure "Header line was not found")) |}]
  >>= fun () ->
  Expect_test_helpers.show_raise (fun () ->
    parse_string ~sep:'|' ~header:(`Limit ["foo"]) "");
  [%expect {| (raised (Failure "Header line was not found")) |}]
;;
