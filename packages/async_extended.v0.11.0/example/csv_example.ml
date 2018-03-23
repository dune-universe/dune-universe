open Core
open Async

module Csv = Async_extended.Std.Csv

let summary = "takes a headerless csv (foo, bar) and converts it to (bar, foo) with header on stdout"

type t =
  { foo : int
  ; bar : string
  }

(* Describes how to generate a [t] from a row of a csv file *)
let builder : t Csv.t =
  let open Csv.Let_syntax in
  let%map_open foo = at_header "foo" ~f:Int.of_string
  and bar = at_header "bar" ~f:String.of_string
  in
  { foo; bar; }
;;

let to_csv_row t =
  let { foo; bar; } = t in
  [ bar; Int.to_string foo; ]

let main =
  let open Command.Let_syntax in
  let%map_open filename = anon ("FILE" %: file) in
  fun () ->
    let writer =
      Async_extended.Delimited.Csv.of_writer
        ~line_breaks:`Unix
        (Lazy.force Writer.stdout)
    in
    Reader.with_file filename ~f:(fun reader ->
      (* Read csv *)
      let t_pipe =
        Csv.fold_reader_to_pipe
          ~header:(`Add [ "foo"; "bar"; ])
          builder
          reader
      in
      (* Write csv *)
      Pipe.write_without_pushback writer [ "bar"; "foo"; ];
      Pipe.iter t_pipe ~f:(fun t -> Pipe.write writer (to_csv_row t))
    )
    >>| fun () ->
    Pipe.close writer
;;

let () = Command.run (Command.async ~summary main)
