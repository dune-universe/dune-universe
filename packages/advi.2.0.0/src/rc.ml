(***********************************************************************)
(*                                                                     *)
(*                             Active-DVI                              *)
(*                                                                     *)
(*                   Projet Cristal, INRIA Rocquencourt                *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Lesser General Public License.          *)
(*                                                                     *)
(*  Jun Furuse, Didier Rémy and Pierre Weis.                           *)
(*  Contributions by Roberto Di Cosmo, Didier Le Botlan,               *)
(*  Xavier Leroy, and Alan Schmitt.                                    *)
(*                                                                     *)
(*  Based on Mldvi by Alexandre Miquel.                                *)
(***********************************************************************)

(* $Id$ *)

(* Init file loading. *)

(* Borrowed from arg.ml *)
type error =
  | Unknown of string
  | Wrong of string * string * string  (* option, actual, expected *)
  | Missing of string
  | Message of string

open Printf

let rec assoc3 x l =
  match l with
  | [] -> raise Not_found
  | (y1, y2, y3) :: t when y1 = x -> y2
  | _ :: t -> assoc3 x t
;;

open Arg;;

let current = Arg.current;;
let usage = Arg.usage;;
type spec = Arg.spec;;

let parse_args progname argv speclist anonfun errmsg =
  let l = Array.length argv in
  let stop error =
    begin match error with
      | Unknown "-help" -> ()
      | Unknown "--help" -> ()
      | Unknown s ->
          eprintf "%s: unknown option `%s'.\n" progname s
      | Missing s ->
          eprintf "%s: option `%s' needs an argument.\n" progname s
      | Wrong (opt, arg, expected) ->
          eprintf "%s: wrong argument `%s'; option `%s' expects %s.\n"
                  progname arg opt expected
      | Message s ->
          eprintf "%s: %s.\n" progname s
    end;
    usage speclist errmsg;
    if error = Unknown "-help" || error = Unknown "--help"
    then exit 0
    else exit 2
  in
  incr current;
  while !current < l do
    let s = argv.(!current) in
    if String.length s >= 1 && String.get s 0 = '-' then begin
      let action =
        try assoc3 s speclist
        with Not_found -> stop (Unknown s)
      in
      begin try
        match action with
        | Unit f -> f ();
        | Set r -> r := true;
        | Clear r -> r := false;
        | String f when !current + 1 < l ->
            let arg = argv.(!current + 1) in
            f arg;
            incr current;
        | Int f when !current + 1 < l ->
            let arg = argv.(!current + 1) in
            begin try f (int_of_string arg) with
            | Failure _ -> stop (Wrong (s, arg, "an integer"))
            end;
            incr current;
        | Float f when !current + 1 < l ->
            let arg = argv.(!current + 1) in
            begin try f (float_of_string arg) with
            | Failure _ -> stop (Wrong (s, arg, "a float"))
            end;
            incr current;
        | Rest f ->
            while !current < l - 1 do
              f argv.(!current + 1);
              incr current;
            done;
        | _ -> stop (Missing s)
      with Bad m -> stop (Message m);
      end;
      incr current;
    end else begin
      (try anonfun s with Bad m -> stop (Message m));
      incr current;
    end;
  done;
;;

let arg_dot_parse argv =
   let progname =
    if !current < Array.length argv then argv.(!current) else "(?)" in
   parse_args progname argv;;

(* Implementation of rcfile input *)
exception Lexical_error of string;;

let string_of_ic ic =
   let b = Buffer.create 1024 in
   begin
     try while true do Buffer.add_channel b ic 1 done
     with End_of_file -> ()
   end;
   Buffer.contents b;;

let string_of_file fname =
  let ic = open_in_bin fname in
  let res = string_of_ic ic in
  close_in ic;
  res;;

let rec next_char i lim s =
  if i >= lim then raise End_of_file else
  match s.[i] with
  | ' ' | '\t' | '\n' | '\r' -> next_char (i + 1) lim s
  | '#' -> next_char (skip_comment (i + 1) lim s) lim s
  | c -> i

and skip_comment i lim s =
  if i >= lim then raise End_of_file else
  match s.[i] with
  | '\n' ->
      let ni = i + 1 in
      if ni >= lim then raise End_of_file else ni
  | c -> skip_comment (i + 1) lim s;;

let lex s =
  let b = Buffer.create 111 in
  let lim = String.length s in
  (* Find_tok is called at the beginning of a token *)
  let rec find_tok i =
    if i >= lim then get_tok i b else
    match s.[i] with
    | '"' (* '"' *) -> find_string (i + 1)
    | ' ' | '\t' | '\n' | '\r' -> get_tok i b
    | '\\' -> find_tok (i + 1)
    | c -> Buffer.add_char b c; find_tok (i + 1)

  and find_string i =
    if i >= lim then raise (Lexical_error "end of string not found") else
    match s.[i] with
    | '"' (* '"' *) -> get_tok (i + 1) b
    | '\\' -> find_escaped_string (i + 1)
    | c -> Buffer.add_char b c; find_string (i + 1)

  and find_escaped_string i =
    if i >= lim
    then raise (Lexical_error "end of string (with escape) not found") else
    match s.[i] with
    | '"' as c (* '"' *) -> Buffer.add_char b c; find_string (i + 1)
    | c -> Buffer.add_char b '\\'; Buffer.add_char b c; find_string (i + 1)
    
  and get_tok i b =
    let tok = Buffer.contents b in
    Buffer.clear b;
    i, tok in

  let rec find_tokens accu i =
    try
      let i = next_char i lim s in
      let ni, tok = find_tok (next_char i lim s) in
      find_tokens (tok :: accu) ni
     with End_of_file -> List.rev accu in
  find_tokens [] 0;;

let argv_of_string s =
  (* Debug if necessary
     let l = lex s in
     print_int (List.length l);
     print_newline (); 
     List.iter (fun s -> print_string s; print_newline ()) l; *)
  Array.of_list (lex s);;

let argv_of_file fname = argv_of_string (string_of_file fname);;

let parse_argv progname argv speclist anonfun errmsg =
 let curr = !current in
 try
   current := -1;
   parse_args progname argv speclist anonfun errmsg;
   current := curr
 with x -> current := curr; raise x;;

let parse_string s = parse_argv s (argv_of_string s);;

let parse_file fname = parse_argv fname (argv_of_file fname);;

let cautious_parse_file fname speclist anonfun errmsg =
 try parse_file fname speclist anonfun errmsg with
 | Failure s
 | Lexical_error s
 | Sys_error s -> Misc.warning (Printf.sprintf "Error while loading %s" s);;

(*
Example:
let usage_msg =
  Printf.sprintf "usage: %s [OPTIONS] DVIFILE" Sys.argv.(0);;

let set_dviname s =
  print_string s; print_newline ();;

let set_dim m dim =
 print_string ("Setting " ^ m ^ " to " ^ dim);
 print_newline ();;

let crop_flag = ref false;;
let set_geom s = 
 print_string ("Setting geometry to " ^ s);
 print_newline ();;

let spec_list = [
  ("-geometry", Arg.String set_geom,
   "GEOM\tSets the (maximum) geometry GEOM");
  ("-g", Arg.String set_geom,
   "GEOM\tSame as -geometry GEOM");
  ("-crop", Arg.Set crop_flag,
   "\tCrop the window to the best size (default)");
  ("-nocrop", Arg.Clear crop_flag,
   "\tDisable cropping");
  ("-nomargins", Arg.Unit
     (fun () -> set_dim "hmargin" "0cm"; set_dim "vmargin" "0cm"),
   "\tSuppress horizontal and vertical margins");
  ("-hmargin", Arg.String (set_dim "hmargin"),
   "DIMEN\tHorizontal margin  (default: 1cm)");
  ("-vmargin", Arg.String (set_dim "vmargin"),
   "DIMEN\tVertical margin    (default: 1cm)");
];;
(*  let sort = List.sort (fun (s1, _, _) (s2, _, _) -> compare s1 s2) in
  Options.pretty (spec_list @ sort (Options.all ()));;*)

parse ".advirc" spec_list set_dviname usage_msg;;
*)

let init_list = ref [];;

let at_init f = init_list := f :: !init_list;;

let init () = List.iter (fun f -> f ()) (List.rev !init_list);;
