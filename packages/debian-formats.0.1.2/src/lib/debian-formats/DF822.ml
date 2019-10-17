(******************************************************************************)
(*  ocaml-debian-formats: parse Debian files.                                 *)
(*                                                                            *)
(*  Copyright (C) 2010-2017, Sylvain Le Gall                                  *)
(*                                                                            *)
(*  This library is free software; you can redistribute it and/or modify it   *)
(*  under the terms of the GNU Lesser General Public License as published by  *)
(*  the Free Software Foundation; either version 2.1 of the License, or (at   *)
(*  your option) any later version, with the OCaml static compilation         *)
(*  exception.                                                                *)
(*                                                                            *)
(*  This library is distributed in the hope that it will be useful, but       *)
(*  WITHOUT ANY WARRANTY; without even the implied warranty of                *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file         *)
(*  COPYING for more details.                                                 *)
(*                                                                            *)
(*  You should have received a copy of the GNU Lesser General Public License  *)
(*  along with this library; if not, write to the Free Software Foundation,   *)
(*  Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA             *)
(******************************************************************************)

(** Low level debian format parser *)

open ExtLib

type name = string

type version = string

type vpkg = string * (string * string) option

type veqpkg = string * (string * string) option

type t = {
  mutable next : unit -> string * int;
  mutable line : int;
  mutable cur : string;
  mutable eof : bool;
}

let print_warning fmt = Printf.ifprintf () fmt

let dummy_t = { next = (fun _ -> ("", 0)); cur = ""; eof = false; line = -1 }

let eof i = i.eof

let cur i =
  assert (not i.eof);
  i.cur

let parse_error ?s i =
  let err_string =
    match s with
    | None when i.line > 0 ->
        Printf.sprintf "Parse error at line %d: \"%s\"" i.line i.cur
    | None -> Printf.sprintf "Parse error : \"%s\"" i.cur
    | Some s when i.line > 0 ->
        Printf.sprintf "Error: %s at line %d: \"%s\"" s i.line i.cur
    | Some s -> Printf.sprintf "Error: %s : \"%s\"" s i.cur
  in
  failwith err_string

let next i =
  assert (not i.eof);
  try
    let s, l = i.next () in
    i.cur <- s;
    i.line <- l
  with End_of_file -> i.eof <- true

let expect s v =
  assert ((not (eof s)) && cur s = v);
  next s

let is_blank i = (not (eof i)) && cur i = ""

let skip_blank_lines i =
  while is_blank i do
    next i
  done

let field_re = Str.regexp "^\\([^:]*\\)*:[ \t]*\\(.*\\)$"

let remove_ws s =
  let l = String.length s in
  let p = ref (l - 1) in
  while !p >= 0 && (s.[!p] = ' ' || s.[!p] = '\t') do
    decr p
  done;
  if !p + 1 = l then s else String.sub s 0 (!p + 1)

(* return None with EOF *)
let parse_paragraph i =
  skip_blank_lines i;
  if eof i then None
  else
    let fields = ref [] in
    while
      let l = cur i in
      if not (Str.string_match field_re l 0) then parse_error i;
      let name = Str.matched_group 1 l in
      let data1 = remove_ws (Str.matched_group 2 l) in
      let data = ref [ data1 ] in
      next i;
      while
        (not (eof i || is_blank i))
        &&
        let l = cur i in
        l.[0] = ' ' || l.[0] = '\t'
      do
        data := remove_ws (cur i) :: !data;
        next i
      done;
      fields := (String.lowercase name, List.rev !data) :: !fields;
      not (eof i || is_blank i)
    do
      ()
    done;
    assert (!fields <> []);
    Some (List.rev !fields)

let single_line f = function
  | [ s ] -> s
  | _ as l ->
      failwith
        (Printf.sprintf "field '%s' should be a single line\n%s" f
           (String.concat " " l))

(* May need to accept package name containing "_" *)
let token_re =
  Str.regexp
    ( "[ \t]+\\|\\("
    ^ String.concat "\\|"
        [
          ",";
          "|";
          "(";
          ")";
          "\\[";
          "\\]";
          "!";
          "<<";
          "<=";
          "=";
          ">=";
          ">>";
          "<";
          ">";
          "[A-Za-z0-9.:_+~-]+";
        ]
    ^ "\\)" )

let rec next_token s p =
  if !p = String.length s then raise End_of_file
  else if Str.string_match token_re s !p then (
    p := Str.match_end ();
    try Str.matched_group 1 s with Not_found -> next_token s p )
  else parse_error ~s:(Printf.sprintf "Bad token in '%s' at %d" s !p) dummy_t

let start_from_fun f =
  let res = { next = f; cur = ""; eof = false; line = 0 } in
  next res;
  res

let start_token_stream s =
  let p = ref 0 in
  start_from_fun (fun () -> (next_token s p, 0))

let start_from_channel =
  let i = ref 0 in
  fun ch ->
    start_from_fun (fun () ->
        try
          incr i;
          (IO.read_line ch, !i)
        with IO.No_more_input -> raise End_of_file)

exception ParseError of string * int

(*****************************************************)

let strict_version_re_1 =
  Str.regexp
    ( "^\\(\\([0-9]+\\):\\)?" ^ "\\([0-9][A-Za-z0-9.:+~-]*\\)"
    ^ "-\\([A-Za-z0-9.+~]+\\)$" )

let strict_version_re_2 =
  Str.regexp ("^\\(\\([0-9]+\\):\\)?" ^ "\\([0-9][A-Za-z0-9.:+~]*\\)\\( \\)?$")

(* Some upstream version do not start with a digit *)
let version_re_1 =
  Str.regexp
    "^\\(\\([0-9]+\\):\\)?\\([A-Za-z0-9._:+~-]+\\)-\\([A-Za-z0-9.+~]+\\)$"

(* XXX add '-' to the version spec ... *)
let version_re_2 =
  Str.regexp "^\\(\\([0-9]+\\):\\)?\\([A-Za-z0-9._:+~-]+\\)\\( \\)?$"

let check_version i s =
  if
    not
      ( Str.string_match strict_version_re_1 s 0
      || Str.string_match strict_version_re_2 s 0 )
  then (
    print_warning "bad version '%s'" s;
    if
      not
        (Str.string_match version_re_1 s 0 || Str.string_match version_re_2 s 0)
    then
      (* parse_error ~s:(Printf.sprintf "Bad version '%s'" s) i *)
      raise (ParseError (Printf.sprintf "Bad version '%s'" s, i.line)) )

let parse_version s =
  ( try check_version dummy_t s
    with ParseError (s, _) -> parse_error ~s dummy_t );
  s

(*****************************************************)

let strict_package_re = Str.regexp "^[a-z0-9][a-z0-9.+-]+$"

let package_re = Str.regexp "^[A-Za-z0-9][A-Za-z0-9._+-]+$"

let check_package_name i s =
  if not (Str.string_match strict_package_re s 0) then (
    print_warning "bad package name '%s'" s;
    if not (Str.string_match package_re s 0) then
      (* parse_error ~s:(Printf.sprintf "Bad package name '%s'" s) i *)
      raise (ParseError (Printf.sprintf "Bad version '%s'" s, i.line)) )

let parse_package s =
  ( try check_package_name dummy_t s
    with ParseError (s, _) -> parse_error ~s dummy_t );
  s

(*****************************************************)

let parse_constr_aux vers s =
  let name = cur s in
  check_package_name s name;
  next s;
  if (not (eof s)) && cur s = "(" then (
    try
      if not vers then
        parse_error
          ~s:(Printf.sprintf "Package version not allowed in '%s'" name)
          s;
      next s;
      let comp = cur s in
      next s;
      let version = cur s in
      check_version s version;
      next s;
      expect s ")";
      (name, Some (comp, version))
    with ParseError (s, _) ->
      Printf.eprintf "WARNING !!! '%s'\n" s;
      (name, None) )
  else
    ( (* XXX if the constraint if malformed I should print a warning !!! *)
      name,
      None )

let parse_constr s =
  let s = start_token_stream s in
  parse_constr_aux true s

let parse_virtual_constr_aux vers s =
  let name = cur s in
  check_package_name s name;
  next s;
  if (not (eof s)) && cur s = "(" then (
    try
      if not vers then
        parse_error
          ~s:(Printf.sprintf "Package version not allowed in '%s'" name)
          s;
      next s;
      let comp = cur s in
      next s;
      let version = cur s in
      check_version s version;
      next s;
      expect s ")";
      (name, Some (comp, version))
    with ParseError (s, _) ->
      Printf.eprintf "WARNING !!! '%s'\n" s;
      (name, None) )
  else
    ( (* XXX if the constraint if malformed I should print a warning !!! *)
      name,
      None )

let parse_virtual_constr s =
  let s = start_token_stream s in
  parse_virtual_constr_aux true s

let parse_builddeps s =
  let s = start_token_stream s in
  let c = parse_constr_aux true s in
  if (not (eof s)) && cur s = "[" then (
    let l = ref [] in
    next s;
    while (not (eof s)) && not (cur s = "]") do
      if (not (eof s)) && cur s = "!" then (
        next s;
        l := (false, cur s) :: !l )
      else l := (true, cur s) :: !l;

      next s
    done;
    (c, !l) )
  else (c, [])

(*****************************************************)

let parse_source s =
  let space_lst =
    List.flatten
      (List.map (fun s -> String.nsplit s " ") (String.nsplit s "\t"))
  in
  match space_lst with
  | [ n ] -> (n, None)
  | [ n; s' ] ->
      let re = Str.regexp "(\\([^)]+\\))" in
      if Str.string_match re s' 0 then (n, Some (Str.matched_group 1 s'))
      else (
        print_warning "bad source name '%s'\n" s;
        (n, None) )
  | _ ->
      parse_error ~s:(Printf.sprintf "Malformed source field : '%s'" s) dummy_t

let and_list_parser p s =
  List.map (fun str -> p (String.strip str)) (String.nsplit s ",")

let parse_vpkglist parse_vpkg = and_list_parser parse_vpkg

let parse_vpkgformula parse_vpkg s =
  List.map
    (fun and_arg ->
      let or_args = String.nsplit and_arg "|" in
      List.map (fun str -> parse_vpkg (String.strip str)) or_args)
    (String.nsplit s ",")

let parse_veqpkglist parse_veqpkg = and_list_parser parse_veqpkg

exception Eof

(** parse a 822 compliant file.
    @return list of packages.
    @param parse : paragraph parser
    @param f : filter to be applied to each paragraph
    @param ExtLib.IO.input channel *)
let parse_822_iter parse f ch =
  let l = ref [] in
  try
    while true do
      match parse_paragraph ch with
      | None -> raise Eof
      | Some par -> (
          match parse par with None -> () | Some e -> l := f e :: !l )
    done;
    !l
  with Eof -> !l
