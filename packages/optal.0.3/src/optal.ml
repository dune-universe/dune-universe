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
  let ppf = Format.err_formatter in
  report_error ppf e;
  Format.pp_print_flush ppf ();
  exit 1

(**** main ****)

let argl = List.tl (Array.to_list Sys.argv)

let opl =
  try Some (List.find (fun s -> Filename.check_suffix s ".opl") argl)
  with Not_found -> None

let json =
  try Some (List.find (fun s -> Filename.check_suffix s ".json") argl)
  with Not_found -> None

let parse_opl (channel, filename) =
  let lexbuf = Lexing.from_channel channel in
  let pos = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <-
    { pos with Lexing.pos_fname = filename };
  let result =
    try
      Parser.file Lexer.token lexbuf
    with
    | Parser.Error ->
      let loc = Loc.curr lexbuf in
      raise (Error (Parse_error loc))
    | e -> raise e
  in
  close_in channel;
  result

let main () =
  let opl_in =
    match opl with
    | None -> raise (Error No_provided_input)
    | Some opl -> (open_in opl, opl) in
  let opl = parse_opl opl_in in
  let init_env =
    match json with
    | None -> Value.Env.empty
    | Some file -> Json_value.load_file (open_in file, file) Value.Env.empty in
  let lp = Generate.to_lp init_env opl in
  Lp.output_file stdout lp

let () = try main () with e -> error e
