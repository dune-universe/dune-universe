open Optal

(**** error printing *****)

type error =
  | No_provided_input
  | Parse_error of Loc.t

exception Error of error

let print_loc_error ppf loc f e =
  Loc.print_loc ppf loc;
  Format.fprintf ppf ":@.Error: %a@." f e

let report_error_main ppf = function
  | No_provided_input ->
    Format.fprintf ppf "Error: No input file provided@."
  | Parse_error loc ->
    Loc.print_loc ppf loc;
    Format.fprintf ppf ":@.Error: parse error@."

let report_error ppf e = match e with
  | Lexer.Error (e,loc) ->
    print_loc_error ppf loc Lexer.report_error e
  | Generate.Error(e,loc) ->
    print_loc_error ppf loc Generate.report_error e
  | Value.Error(e,loc) ->
    print_loc_error ppf loc Value.report_error e
  | Json_value.Error(e,filename) ->
    Json_value.report_error ppf filename e
  | Lp.Error(e) ->
    Lp.report_error ppf e
  | Error e ->
    report_error_main ppf e
  | e -> raise e

let error e =
  let b = Buffer.create 100 in
  let ppf = Format.formatter_of_buffer b in
  report_error ppf e;
  Format.pp_print_flush ppf ();
  let s = Buffer.contents b in
  s

(**** main ****)

let parse_opl s =
  let lexbuf = Lexing.from_string s in
  let pos = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <-
    { pos with Lexing.pos_fname = "input" };
  try
    Parser.file Lexer.token lexbuf
  with
  | Parser.Error ->
    let loc = Loc.curr lexbuf in
    raise (Error (Parse_error loc))
  | e -> raise e

let lp_to_string lp =
  let b = Buffer.create 100 in
  Lp.print_file b lp;
  Buffer.contents b

let run_optal' () =
  let opl_in = Utils.get_optal () in
  let data = Utils.get_data () in
  Utils.debug "parse opl";
  let opl = parse_opl opl_in in
  Utils.debug "parse json";
  let init_env = match data with
    | "" -> Value.Env.empty
    | _ -> Json_value.load_string data Value.Env.empty in
  Utils.debug "generate lp";
  let lp = Generate.to_lp init_env opl in
  Utils.debug "convert lp";
  lp_to_string lp

type 'a result =
  | Ok of 'a
  | Error of string

let run_optal () =
  try Ok (run_optal' ()) with e -> Error (error e)

let run () =
  match run_optal () with
  | Error e ->
    Utils.set_output e;
    Utils.debug "optal error: %s" e
  | Ok s -> Utils.set_output s

open Lwt

let () =
  ignore_result
    (Lwt_js_events.clicks Utils.go_button
       (fun _ _ ->
          Utils.debug "run";
          run ();
          Lwt.return ()))
