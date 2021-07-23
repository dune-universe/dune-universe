(**************************************************************************************)
(*  Copyright (C) 2021 Johannes Schauer Marin Rodrigues <josch@mister-muffin.de>      *)
(*                                                                                    *)
(*  This library is free software: you can redistribute it and/or modify              *)
(*  it under the terms of the GNU Lesser General Public License as                    *)
(*  published by the Free Software Foundation, either version 3 of the                *)
(*  License, or (at your option) any later version.  A special linking                *)
(*  exception to the GNU Lesser General Public License applies to this                *)
(*  library, see the COPYING file for more information.                               *)
(**************************************************************************************)

open ExtLib
open Dose_common
open Dose_extra
open Dose_doseparse

include Util.Logging (struct
  let label = "dose_applications.debGrep"
end)

exception ParseError of string

let main () =
  if Array.length Sys.argv = 1 || Sys.argv.(1) = "--help" then (
    Printf.printf "%s: [options] [pattern]\n" Sys.argv.(0) ;
    0)
  else
    (* we don't need a lexer because our tokens are already in Sys.argv *)
    let stream = Stream.of_list (List.tl (Array.to_list Sys.argv)) in
    let lexbuf = Lexing.from_string "" in
    let lexer lexbuf =
      (* we abuse lex_curr_p for storing the current argument position *)
      lexbuf.Lexing.lex_start_p <- Lexing.dummy_pos ;
      lexbuf.Lexing.lex_curr_p <-
        { Lexing.dummy_pos with pos_cnum = Stream.count stream + 1 } ;
      try
        match Stream.next stream with
        | "-q" | "--quiet" | "--silent" -> Grep_argv_parser.QUIET
        | "--show-field" | "-s" -> Grep_argv_parser.SHOW_FIELD
        | "--no-field-names" | "-n" -> Grep_argv_parser.NO_FIELD_NAMES
        | "--field" | "-F" -> Grep_argv_parser.FIELD
        | "--exact-match" | "-X" -> Grep_argv_parser.EXACT
        | "--not" | "--!" | "!" -> Grep_argv_parser.NOT
        | "--or" | "-o" -> Grep_argv_parser.OR
        | "--and" | "-a" -> Grep_argv_parser.AND
        | "(" -> Grep_argv_parser.LPAREN
        | ")" -> Grep_argv_parser.RPAREN
        | v -> Grep_argv_parser.NAME v
      with Stream.Failure -> Grep_argv_parser.EOL
    in
    (* as the grep-dctrl filter syntax is its own language, we parse it using
     * ocamlyacc *)
    let argv =
      try Grep_argv_parser.argv lexer lexbuf
      with Parsing.Parse_error ->
        let pos = lexbuf.Lexing.lex_curr_p.pos_cnum in
        if pos >= Array.length Sys.argv then
          raise (ParseError (Printf.sprintf "Unexpected end of filter options"))
        else
          raise
            (ParseError
               (Printf.sprintf
                  "Cannot parse filter at option %d: %s"
                  pos
                  Sys.argv.(pos)))
    in
    let (quiet, showfield, nofieldnames, expr) =
      match argv with
      | Grep_argv_types.Argv (o, e) ->
          ( List.mem Grep_argv_types.Quiet o,
            List.fold_left
              (fun acc el ->
                match el with
                | Grep_argv_types.ShowField s ->
                    (* multiple --show-field options are concatenated *)
                    ExtString.String.nsplit s "," @ acc
                | _ -> acc)
              []
              o,
            List.mem Grep_argv_types.NoFieldNames o,
            e )
    in
    let matches =
      try
        Format822.parse_from_ch
          (fun p ->
            let rec packages_parser_aux n p =
              match
                Format822_parser.stanza_822
                  Format822_lexer.token_822
                  p.Format822.lexbuf
              with
              | None -> n
              | Some stanza ->
                  let rec matches ?(exact = false) = function
                    | Grep_argv_types.Field (fieldname, pattern) ->
                        List.exists
                          (fun (k, (_, v)) ->
                            if k = fieldname then
                              if exact then pattern = v
                              else ExtString.String.exists v pattern
                            else false)
                          stanza
                    | Grep_argv_types.And (e1, e2) ->
                        matches ~exact e1 && matches ~exact e2
                    | Grep_argv_types.Or (e1, e2) ->
                        matches ~exact e1 || matches ~exact e2
                    | Grep_argv_types.Not e -> not (matches ~exact e)
                    | Grep_argv_types.Exact e -> matches ~exact:true e
                  in
                  if matches expr then (
                    let print k v =
                      if quiet then ()
                      else if nofieldnames then Printf.printf "%s\n" v
                      else Printf.printf "%s: %s\n" k v
                    in
                    (match showfield with
                    | [] ->
                        List.iter (fun (k, (_, v)) -> print k v) stanza ;
                        print_string "\n"
                    | [field] ->
                        (*when printing a single field, don't print empty lines between stanzas*)
                        List.iter
                          (fun (k, (_, v)) -> if k = field then print k v)
                          stanza
                    | fields ->
                        List.iter
                          (fun (k, (_, v)) ->
                            if List.mem k fields then print k v)
                          stanza ;
                        print_string "\n") ;
                    packages_parser_aux (n + 1) p)
                  else packages_parser_aux n p
            in
            packages_parser_aux 0 p)
          (IO.input_channel stdin)
      with Format822.ParseError (cl, label, errmsg) ->
        fatal
          "Filename %s\n %s\n %s : %s"
          "filename"
          (String.concat "\n " cl)
          label
          errmsg
    in
    if matches > 0 then 0 else 1
;;

StdUtils.if_application ~alternatives:["dose-debgrep"] "debGrep" main
