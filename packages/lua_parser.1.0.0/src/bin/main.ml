(*
 The MIT License                                                                                                                                 
                                                                                                                                                 
 Copyright (c) 2020 Jason D. Nielsen <drjdnielsen@gmail.com>
 *)

open Lua_parser
open Pp_ast
open Pp_lua
open Parse

let main () =
  let hmesg =
     "\n\
       Usage: l2l [options] file\n\
       Options:\n\
      \  -lua      Pretty prints code back to lua\n\
      \  -sexp     Pretty prints code as an S-expression\n\
      \  -show     Pretty prints code using Ocaml's pretty printer\n\
      \  -help     Prints this message\n"
  in
  begin
    if Array.length Sys.argv = 2 then
      match Sys.argv.(1) with
      | "-help" | "-h" -> print_string hmesg; exit 0
      | _ -> failwith "Unknown option and missing file to parse (see -help)!"
  end;
  let cin =
    if Array.length Sys.argv > 2 then 
      open_in Sys.argv.(2)
    else 
       failwith "A file to parse and an option need to be specified (see -help)!"
  in
  let flags = Sys.argv.(1) in
  let ast = parse_from_chan cin in
  match flags with
  | "-lua" -> pp_lua ast
  | "-sexp" -> pp_ast_sexp ast
  | "-show" -> pp_ast_show ast
  | "-help" | "-h" -> print_string hmesg
  | _ -> failwith "Option unknown (see -help)!"


let _ = main ()
