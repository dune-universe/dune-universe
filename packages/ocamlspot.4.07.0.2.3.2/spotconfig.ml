(***********************************************************************)
(*                                                                     *)
(*                            OCamlSpotter                             *)
(*                                                                     *)
(*                             Jun FURUSE                              *)
(*                                                                     *)
(*   Copyright 2008-2014 Jun Furuse. All rights reserved.              *)
(*   This file is distributed under the terms of the GNU Library       *)
(*   General Public License, with the special exception on linking     *)
(*   described in file LICENSE.                                        *)
(*                                                                     *)
(***********************************************************************)

(* Versions and command line options *)

open Format
open Ext
open Utils

open Spot

let app_version = Spot.version

let version =
  Printf.sprintf "%s for ocaml %s" app_version Spot.ocaml_version

let print_version () =
  eprintf "ocamlspot %s@." version
    
let rev_anonargs           = ref []
let dump_file              = ref false
let dump_rannots           = ref false
let dump_tree              = ref false
let dump_top               = ref false
let dump_flat              = ref false
let eager_dump             = ref false
let no_definition_analysis = ref false
let strict_time_stamp      = ref false
let print_interface        = ref false
let type_expand            = ref false
let rest_args_rev          = ref []
let code_test              = ref false

let _ = 
  Arg.parse (Arg.align
    [ "--version", 
      Arg.Unit print_version, " : print version information";

      "-n", 
      Arg.Set no_definition_analysis, " : no definition analysis";

      "--no-analysis", 
      Arg.Set no_definition_analysis, " : no definition analysis";
      
      "--strict-time-stamp", 
      Arg.Set strict_time_stamp, " : error at newer source files than their spots";

      "--interface", 
      Arg.Set print_interface, " : show the interface rather than the definition (experimental)";

      "--type-expand",
      Arg.Set type_expand, " : expression/pattern expansion by type (experimental)";

      "--debug", 
      Arg.Set Debug.on, " : print debug information";

      "--dump-file", 
      Arg.Set dump_file, " : dump spot file"; 

      "--dump-rannots", 
      Arg.Set dump_rannots, " : dump loc-annots";

      "--dump-tree", 
      Arg.Set dump_tree, " : dump annot tree";

      "--dump-top", 
      Arg.Set dump_top, " : dump top"; 

      "--dump-flat", 
      Arg.Set dump_flat, " : dump flat"; 

      "--eager-dump", 
      Arg.Set eager_dump, " : eager evaluation at dump";

      "--code-test",
      Arg.Set code_test, " : test some library functions";
    ])
    (fun s -> rev_anonargs := s :: !rev_anonargs)
    (Printf.sprintf 
        "ocamlspot version %s\n\
\n\
Synopsis:\n\
\tDefinition query:\n\
\t\tocamlspot <search>\n\
\t\tocamlspot query <search>\n\
\n\
\tUse query:\n\
\t\tocamlspot use <search> <targets>\n\
\n\
\tDisplay entire signature:\n\
\t\tocamlspot mli <file>\n\
\n\
\tType check and spot creation:\n\
\t\tocamlspot typecheck <args>\n\
\n\
\tRetype check and spot recreation:\n\
\t\tocamlspot recheck <targets>\n\
\n\
\tDump various information:\n\
\t\tocamlspot --dump-<style> <file>\n\
\n\
\t<search> ::= <file>:<pos> | <file>:<kind>:<path>\n\
\t<pos> ::= l<line>c<column> | b<bytes>\n\
\t<kind> ::= v|t|e|m|mt|c|ct\n\
\n\
Options:"
        version)

let dump_file              = !dump_file
let dump_rannots           = !dump_rannots
let dump_tree              = !dump_tree
let dump_top               = !dump_top 
let dump_flat              = !dump_flat
let eager_dump             = !eager_dump
let no_definition_analysis = !no_definition_analysis
let strict_time_stamp      = !strict_time_stamp
let print_interface        = !print_interface
let type_expand            = !type_expand
let code_test              = !code_test

let dump_any = 
  dump_file || dump_rannots || dump_tree || dump_top || dump_flat

module SearchSpec = struct
  type t = 
      | Pos of Position.t
      | Kind of Kind.t * Path.t

  let parse s : string * t =
    try
      let at = String.rindex s ':' in
      try
        let at2 = String.rindex_from s (at - 1) ':' in
        String.sub s 0 at2,
        let kind = Kind.from_string (String.sub s (at2+1) (at-at2-1)) in
        let path_string = String.sub s (at+1) (String.length s - at - 1) in
        let path = 
          try Path.parse s with
          | _ -> failwithf "illegal path in <file>:<kind>:<path> : %s" path_string
        in 
        Kind (kind, path)
      with
      | Invalid_argument _ | Not_found -> 
          String.sub s 0 at,
          Pos 
		  (Position.parse 
                     (String.sub s (at+1) (String.length s - at - 1)))
    with
    | Failure s -> failwith s
    | Position.Parse_failure s -> failwithf "illegal <file>:<pos> : %s" s
    | Not_found -> failwithf "strange search : %s" s

  let to_string = function
    | Pos pos -> ":" ^ Position.to_string pos
    | Kind (k, path) -> 
        Printf.sprintf ":%s:%s"
          (String.capitalize_ascii (Kind.to_string k))
          (Path.name path)
end

let _rest_args = List.rev !rest_args_rev (* CR jfuruse: unused?! *)
let anonargs = List.rev !rev_anonargs

let mode =
  if code_test then `CodeTest
  else if dump_any then begin 
    match anonargs with
    | [ spec ] -> `Dump spec
    | _ -> failwith "You cannot specify mode with --dump"
  end else begin
    Debug.format "anonargs = [%a]@." 
      (Format.list " " pp_print_string) 
      anonargs;
    match anonargs with
    | [ "query"; spec ]        -> `Query (SearchSpec.parse spec)
    | [ "info"; file ]         -> `Info file
    | [ "mli"; file ]          -> `Interface file
    | "use" :: spec :: targets -> `Use (SearchSpec.parse spec, targets)
    | "typecheck" :: rest      -> `Typecheck rest
    | "recheck" :: rest        -> `Recheck rest
    | [ spec ]                 -> `Query (SearchSpec.parse spec)
    | _                        -> failwith "At most one search spec is allowed"
  end
