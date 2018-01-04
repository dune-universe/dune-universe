open! Core
open! Async

module Delimited = struct
  module Header = Header

  module Row = Csv.Row

  module Csv = struct
    open Csv
    exception Bad_csv_formatting = Csv.Bad_csv_formatting

    let manual_parse_data parse_state input =
      let parse_state =
        match input with
        | `Eof -> Parse_state.finish parse_state
        | `Data s -> Parse_state.input_string parse_state s
      in
      let queue = Parse_state.acc parse_state in
      let result = Fast_queue.to_list queue in
      Fast_queue.clear queue;
      Second parse_state, result
    ;;

    let create_parse_state ?strip ?sep ?quote header_map =
      Parse_state.create ?strip ?sep ?quote
        ~fields_used:None
        ~init:(Fast_queue.create ())
        ~f:(fun _ queue row ->
          Fast_queue.enqueue queue (Row.create_of_fq header_map row);
          queue)
        ()
    ;;

    let manual_parse_header ?strip ?sep ?quote header_state input =
      let input =
        match input with
        | `Eof -> ""
        | `Data s -> s
      in
      match Header_parse.input_string header_state ~len:(String.length input) input with
      | First header_state -> First header_state, []
      | Second (header_map, input) ->
        let state = create_parse_state ?strip ?sep ?quote header_map in
        manual_parse_data state (`Data input)
    ;;

    let create_manual ?strip ?sep ~header () =
      let state =
        Header_parse.create ?strip ?sep ~header (Builder.return ())
        |> Either.Second.map ~f:(create_parse_state ?strip ?sep)
        |> ref
      in
      let parse_chunk input =
        let state', results =
          match !state with
          | First  state -> manual_parse_header ?strip ?sep state input
          | Second state -> manual_parse_data               state input
        in
        state := state';
        results
      in
      stage parse_chunk
    ;;

    let of_reader ?strip ?skip_lines ?sep ~header reader =
      fold_reader_to_pipe ?strip ?skip_lines ?sep ~header Row.builder reader
    ;;

    let create_reader ?strip ?skip_lines ?sep ~header filename =
      Reader.open_file filename
      >>| of_reader ?strip ?skip_lines ?sep ~header
    ;;

    let parse_string ?strip ?sep ~header csv_string =
      fold_string ?strip ?sep ~header
        Row.builder
        csv_string
        ~init:(Fast_queue.create ())
        ~f:(fun queue row -> Fast_queue.enqueue queue row; queue)
      |> Fast_queue.to_list
    ;;

    let of_writer = Deprecated_csv.of_writer
    let create_writer = Deprecated_csv.create_writer

  end
  let upgrade_delimited_row_pipe r =
    Pipe.folding_map r ~init:None ~f:(fun header_map row ->
      let row = Row.upgrade ?header_map row in
      (Some (Row.header_map row), row))
  ;;

  module Positional = struct
    include Positional

    let of_reader ?strip ?skip_lines ?on_parse_error ~header ?strict r =
      Positional.of_reader ?strip ?skip_lines ?on_parse_error ~header ?strict r
      |> Or_error.map ~f:upgrade_delimited_row_pipe
    ;;

    let create_reader ?strip ?skip_lines ?on_parse_error ~header ?strict filename =
      Positional.create_reader ?strip ?skip_lines ?on_parse_error ~header
        ?strict filename
      >>| Or_error.map ~f:upgrade_delimited_row_pipe
    ;;
  end

  let of_reader ?strip ?skip_lines ?on_parse_error ~header ?quote ~sep r =
    Character_separated_without_quoting.of_reader
      ?strip ?skip_lines ?on_parse_error ~header ?quote ~sep r
    |> upgrade_delimited_row_pipe
  ;;

  let create_reader ?strip ?skip_lines ?on_parse_error ~header ?quote ~sep filename =
    Character_separated_without_quoting.create_reader
      ?strip ?skip_lines ?on_parse_error ~header ?quote ~sep filename
    >>| upgrade_delimited_row_pipe
  ;;

end
