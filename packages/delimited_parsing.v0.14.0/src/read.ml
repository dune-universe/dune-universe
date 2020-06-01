open Core
open Async
include Delimited_kernel.Read
open Deferred.Let_syntax
open! Int.Replace_polymorphic_compare

(* the maximum read/write I managed to get off of a socket or disk was 65k *)
let buffer_size = 10 * 65 * 1024

let fold_reader'
      ?strip
      ?(skip_lines = 0)
      ?sep
      ?quote
      ?header
      ?on_invalid_row
      builder
      ~init
      ~f
      r
  =
  let%bind () = Shared.drop_lines r skip_lines in
  match%bind
    match Expert.Parse_header.create ?strip ?sep ?quote ?header () with
    | Second header_map -> return (Some (header_map, None))
    | First header_parse ->
      let buffer = Bytes.create buffer_size in
      Deferred.repeat_until_finished header_parse (fun header_parse ->
        match%bind Reader.read r buffer ~len:buffer_size with
        | `Eof ->
          let newline = "\n" in
          (match
             Expert.Parse_header.input_string
               header_parse
               ~len:(String.length newline)
               newline
           with
           | First (_ : Expert.Parse_header.t) ->
             let%map () = Reader.close r in
             failwith "Header is incomplete"
           | Second (headers, input) -> return (`Finished (Some (headers, Some input))))
        | `Ok len ->
          return
            (match Expert.Parse_header.input header_parse ~len buffer with
             | First header_parse -> `Repeat header_parse
             | Second (headers, input) -> `Finished (Some (headers, Some input))))
  with
  | None -> return init
  | Some (header_map, trailing_input) ->
    let state =
      Expert.create_parse_state
        ?strip
        ?sep
        ?quote
        ?on_invalid_row
        ~header_map
        builder
        ~init:(Queue.create ())
        ~f:(fun queue elt ->
          Queue.enqueue queue elt;
          queue)
    in
    let state =
      Option.fold trailing_input ~init:state ~f:(fun state input ->
        Expert.Parse_state.input_string state input)
    in
    let buffer = Bytes.create buffer_size in
    Deferred.repeat_until_finished (state, init) (fun (state, init) ->
      match%bind Reader.read r buffer ~len:buffer_size with
      | `Eof ->
        let state = Expert.Parse_state.finish state in
        let%bind init = f init (Expert.Parse_state.acc state) in
        let%map () = Reader.close r in
        `Finished init
      | `Ok i ->
        let state = Expert.Parse_state.input state buffer ~len:i in
        let%map init = f init (Expert.Parse_state.acc state) in
        Queue.clear (Expert.Parse_state.acc state);
        `Repeat (state, init))
;;

let bind_without_unnecessary_yielding x ~f =
  match Deferred.peek x with
  | Some x -> f x
  | None -> Deferred.bind x ~f
;;

let fold_reader ?strip ?skip_lines ?sep ?quote ?header ?on_invalid_row builder ~init ~f r
  =
  fold_reader'
    ?strip
    ?skip_lines
    ?sep
    ?quote
    ?header
    ?on_invalid_row
    builder
    ~init
    r
    ~f:(fun acc queue ->
      Queue.fold queue ~init:(return acc) ~f:(fun deferred_acc row ->
        bind_without_unnecessary_yielding deferred_acc ~f:(fun acc -> f acc row)))
;;

let fold_reader_without_pushback
      ?strip
      ?skip_lines
      ?sep
      ?quote
      ?header
      ?on_invalid_row
      builder
      ~init
      ~f
      r
  =
  fold_reader'
    ?strip
    ?skip_lines
    ?sep
    ?quote
    ?header
    ?on_invalid_row
    builder
    ~init
    r
    ~f:(fun acc queue -> return (Queue.fold queue ~init:acc ~f))
;;

let pipe_of_reader ?strip ?skip_lines ?sep ?quote ?header ?on_invalid_row builder reader =
  let r, w = Pipe.create () in
  let write_to_pipe : unit Deferred.t =
    let%bind () =
      fold_reader'
        ?strip
        ?skip_lines
        ?sep
        ?quote
        ?header
        ?on_invalid_row
        builder
        ~init:()
        reader
        ~f:(fun () queue ->
          if Pipe.is_closed w
          then (
            let%bind () = Reader.close reader in
            Deferred.never ())
          else Pipe.transfer_in w ~from:queue)
    in
    return (Pipe.close w)
  in
  don't_wait_for write_to_pipe;
  r
;;

let create_reader ?strip ?skip_lines ?sep ?quote ?header ?on_invalid_row builder filename
  =
  Reader.open_file filename
  >>| pipe_of_reader ?strip ?skip_lines ?sep ?quote ?header ?on_invalid_row builder
;;
