(* This file is free software, copyright Simon Cruanes. See file "LICENSE" for more details. *)

(** {1 SMTLIB parser} *)

module V_2_6 = struct
  module Ast = Ast
  module Loc = Loc
  module Parser = Parser
  module Lexer = Lexer

  let with_in_ file f =
    let ic = open_in file in
    try
      let x = f ic in
      close_in ic;
      x
    with e ->
      close_in_noerr ic;
      raise e

  let parse_list_exn lexbuf =
    try
      Parser.parse_list Lexer.token lexbuf
    with
    | Parsing.Parse_error ->
      raise (Ast.Parse_error (Some (Loc.of_lexbuf lexbuf), "syntax error"))

  let parse_list lexbuf =
    try Ok (parse_list_exn lexbuf)
    with e -> Result.Error (Printexc.to_string e)

  let parse_chan_exn ?(filename="<no name>") ic =
    let lexbuf = Lexing.from_channel ic in
    Loc.set_file lexbuf filename;
    parse_list_exn lexbuf

  let parse_chan ?filename ic =
    try Result.Ok (parse_chan_exn ?filename ic)
    with e -> Result.Error (Printexc.to_string e)

  let parse_file_exn file : Ast.statement list =
    with_in_ file (parse_chan_exn ~filename:file)

  let parse_file file =
    try Result.Ok (parse_file_exn file)
    with e -> Result.Error (Printexc.to_string e)

  let parse_string_exn s =
    let lexbuf = Lexing.from_string s in
    Loc.set_file lexbuf "<memory>";
    parse_list_exn lexbuf

  let parse_string s =
    try Result.Ok (parse_string_exn s)
    with e -> Result.Error (Printexc.to_string e)
end
