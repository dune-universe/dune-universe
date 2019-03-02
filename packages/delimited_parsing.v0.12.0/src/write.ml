open Async

include (
  Delimited_kernel.Write :
    module type of Delimited_kernel.Write
  with module By_row := Delimited_kernel.Write.By_row)

module By_row = struct
  include Delimited_kernel.Write.By_row

  let write_field ~sep w field = Writer.write w (Expert.maybe_escape_field ~sep field)

  let rec write_line ~sep ~line_break w line =
    match line with
    | [] -> Writer.write w line_break
    | [ field ] ->
      write_field ~sep w field;
      write_line ~sep ~line_break w []
    | field :: rest ->
      write_field ~sep w field;
      Writer.write_char w sep;
      write_line ~sep ~line_break w rest
  ;;

  let of_writer ?(sep = ',') ?(line_breaks = `Windows) writer =
    let line_break =
      match line_breaks with
      | `Unix -> "\n"
      | `Windows -> "\r\n"
    in
    let pipe_r, pipe_w = Pipe.create () in
    don't_wait_for (Writer.transfer writer pipe_r (write_line ~sep ~line_break writer));
    upon (Pipe.closed pipe_w) (fun () -> don't_wait_for (Writer.close writer));
    pipe_w
  ;;

  let create_writer ?sep ?line_breaks filename =
    let open Deferred.Let_syntax in
    let%map w = Writer.open_file filename in
    of_writer ?sep ?line_breaks w
  ;;
end

let map_writer ~write_header ~pipe_writer_f builder =
  Pipe.create_writer (fun reader ->
    let%bind pipe_writer = pipe_writer_f () in
    let%bind () =
      if write_header then Pipe.write pipe_writer (headers builder) else Deferred.unit
    in
    Pipe.transfer reader pipe_writer ~f:(to_columns builder))
;;

let of_writer ?sep ?line_breaks ~write_header builder writer =
  let pipe_writer_f () = Deferred.return (By_row.of_writer ?sep ?line_breaks writer) in
  map_writer ~write_header ~pipe_writer_f builder
;;

let create_writer ?sep ?line_breaks ~write_header builder filename =
  let pipe_writer_f () = By_row.create_writer ?sep ?line_breaks filename in
  map_writer ~write_header ~pipe_writer_f builder
;;
