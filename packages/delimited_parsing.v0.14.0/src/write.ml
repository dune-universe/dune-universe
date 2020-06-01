open Core_kernel
open Async
include Delimited_kernel.Write

module Raw = struct
  let of_writer ~init ~f writer =
    Pipe.create_writer (fun reader ->
      init writer;
      Writer.transfer writer reader (f ~writer))
  ;;

  let of_writer_and_close ~init ~f writer =
    let pipe = of_writer ~init ~f writer in
    don't_wait_for
      (let%bind () = Pipe.closed pipe in
       Writer.close writer);
    pipe
  ;;

  let create_writer filename ~init ~f =
    let%map writer = Writer.open_file filename in
    of_writer_and_close writer ~init ~f
  ;;
end

module Expert = struct
  include Delimited_kernel.Write.Expert

  module By_row = struct
    let write_field ~sep w field = Writer.write w (maybe_escape_field ~sep field)

    let write_line ?(sep = ',') ?(line_breaks = `Windows) ~writer line =
      let line_breaks =
        match line_breaks with
        | `Unix -> "\n"
        | `Windows -> "\r\n"
      in
      let rec loop line =
        match line with
        | [] -> Writer.write writer line_breaks
        | [ field ] ->
          write_field ~sep writer field;
          loop []
        | field :: rest ->
          write_field ~sep writer field;
          Writer.write_char writer sep;
          loop rest
      in
      loop line
    ;;

    let base ?sep ?line_breaks create =
      create ~init:(Fn.const ()) ~f:(write_line ?sep ?line_breaks)
    ;;

    let of_writer_and_close ?sep ?line_breaks writer =
      base ?sep ?line_breaks (Raw.of_writer_and_close writer)
    ;;

    let of_writer ?sep ?line_breaks writer =
      base ?sep ?line_breaks (Raw.of_writer writer)
    ;;

    let create_writer ?sep ?line_breaks filename =
      base ?sep ?line_breaks (Raw.create_writer filename)
    ;;
  end

  let base ?sep ?line_breaks ~builder ~write_header create =
    let init =
      if write_header
      then fun writer -> By_row.write_line ?sep ?line_breaks ~writer (headers builder)
      else fun (_ : Writer.t) -> ()
    in
    let f ~writer line =
      By_row.write_line ?sep ?line_breaks ~writer (to_columns builder line)
    in
    create ~init ~f
  ;;

  let of_writer ?sep ?line_breaks ~write_header builder writer =
    base ?sep ?line_breaks ~write_header ~builder (Raw.of_writer writer)
  ;;

  let of_writer_and_close ?sep ?line_breaks ~write_header builder writer =
    base ?sep ?line_breaks ~write_header ~builder (Raw.of_writer_and_close writer)
  ;;

  let create_writer ?sep ?line_breaks ~write_header builder filename =
    base ?sep ?line_breaks ~write_header ~builder (Raw.create_writer filename)
  ;;
end

let protect ~f pipe =
  Monitor.protect
    (fun () -> f pipe)
    ~finally:(fun () ->
      Pipe.close pipe;
      Deferred.ignore_m (Pipe.upstream_flushed pipe))
;;

module By_row = struct
  include Delimited_kernel.Write.By_row

  let with_writer ?sep ?line_breaks writer ~f =
    let pipe = Expert.By_row.base ?sep ?line_breaks (Raw.of_writer writer) in
    protect pipe ~f
  ;;

  let with_file ?sep ?line_breaks filename ~f =
    Writer.with_file filename ~f:(fun writer -> with_writer ?sep ?line_breaks writer ~f)
  ;;
end

let with_writer ?sep ?line_breaks ~write_header builder writer ~f =
  let pipe =
    Expert.base ?sep ?line_breaks ~write_header ~builder (Raw.of_writer writer)
  in
  protect ~f pipe
;;

let with_file ?sep ?line_breaks ~write_header builder filename ~f =
  Writer.with_file filename ~f:(fun writer ->
    with_writer ?sep ?line_breaks ~write_header builder writer ~f)
;;
