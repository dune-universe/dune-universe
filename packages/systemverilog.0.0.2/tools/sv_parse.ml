open Systemverilog
open Lexer
open Lexing

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.sv Lexer.read lexbuf with
  | SyntaxError msg ->
    Printf.fprintf stderr "%a: %s\n" print_position lexbuf msg;
    None
  | Parser.Error ->
    Printf.fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)

let rec parse_and_print lexbuf =
  match parse_with_error lexbuf with
  | Some value -> Grammar.Description.print value;
    parse_and_print lexbuf
  | None -> ()

let loop filename =
  let dsc = Unix.openfile filename [ Unix.O_RDONLY ] 0o640 in
  let inx = Unix.in_channel_of_descr dsc in
  let buf = Lexing.from_channel inx in
  buf.lex_curr_p <- { buf.lex_curr_p with pos_fname = filename };
  parse_and_print buf;
  Unix.close dsc

let fn = ref None

let handle_anon anon =
  match !fn with
  | None -> fn := Some anon
  | Some (_) -> Arg.Bad "filename already set" |> raise

let () =
  Arg.parse [] handle_anon "SystemVerilog test parser";
  match !fn with
  | None -> Printf.fprintf stderr "No file specified"
  | Some (fn) -> loop fn
