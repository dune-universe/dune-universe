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

open DFUtils
open ExtString

type entry = {
  source : string;
  version : string;
  distributions : string list;
  optional_fields : (string * string) list;
  urgency : string;
  maintainer : string;
  timestamp : string;
  changes : string;
}

let skip_line_re =
  Re.(
    compile
      (seq
         [
           bol;
           alt
             [
               seq
                 [ opt (seq [ str ";;"; rep space ]); str "Local variables:" ];
               str "vim:";
               str "# ";
               seq [ str "/*"; non_greedy (rep any); str "*/" ];
               seq [ rep space; eol ];
             ];
         ]))

(* Skip lines until it reachs the end of [ch]. Returns [Some _] if a matching
   line is found and None if end is reached.
 *)
let skip_line ?fst ch =
  let rec getl count =
    let l = try Some (IO.read_line ch) with IO.No_more_input -> None in
    match l with
    | Some line -> skip_line' (count + 1) line
    | None -> (count, None)
  and skip_line' count line =
    if Re.execp skip_line_re line then getl count else (count, Some line)
  in
  match fst with Some line -> skip_line' 0 line | None -> getl 0

let optional_fields_re =
  Re.(
    compile
      (seq
         [
           bol;
           group (rep1 (alt [ alpha; char '-' ]));
           (* key *)
             char '=';
           rep space;
           group (rep any);
         ]))

(* value *)

let header_re =
  let name_chars = Re.(alt [ char '-'; char '+'; char '.'; alpha ]) in
  Re.(
    compile
      (seq
         [
           bol;
           group (seq [ alpha; rep name_chars ]);
           (* source *)
             str " (";
           group (non_greedy (rep any));
           char ')';
           (* version *)
             group (rep1 (seq [ rep1 space; rep1 name_chars ]));
           (* distribs *)
             char ';';
           rep space;
           group (rep any);
         ]))

(* extra_fields *)

let trailer_re =
  let digitn n = Re.(repn digit n (Some n)) in
  Re.(
    compile
      (seq
         [
           bol;
           str " -- ";
           group (rep any);
           (* maint *)
             rep1 space;
           char '<';
           group (rep any);
           char '>';
           (* mail *)
             rep1 space;
           group
             (seq
                [
                  (* "Thu, " *)
                    opt (seq [ rep1 alpha; char ','; rep space ]);
                  (* "18 " *)
                    repn digit 1 (Some 2);
                  rep1 space;
                  (* "May " *)
                    rep1 alpha;
                  rep1 space;
                  (* "2017 " *)
                    digitn 4;
                  rep1 space;
                  (* "00:24:12 " *)
                    digitn 2;
                  char ':';
                  digitn 2;
                  char ':';
                  digitn 2;
                  rep1 space;
                  (* "+0200" *)
                    alt [ char '-'; char '+' ];
                  digitn 4;
                  opt (seq [ rep1 space; char '('; any; char ')' ]);
                ]);
           (* timestamp *)
             rep space;
         ]))

let parse_one ch fst =
  let buff = Buffer.create 13 in
  let next ?fst where f =
    match skip_line ?fst ch with
    | 0, Some line ->
        failwith (Printf.sprintf "Badly formatted %s line: '%s'" where line)
    | _, Some line -> f line
    | _, None ->
        failwith
          (Printf.sprintf "Unexpected end of file when parsing %s" where)
  in
  let parse_optional_fields str =
    let lst =
      Re.(split (compile (seq [ rep space; char ','; rep space ]))) str
    in
    let lst' =
      List.map
        (fun str ->
          try
            let grps = Re.exec optional_fields_re str in
            (Re.Group.get grps 1, Re.Group.get grps 2)
          with Not_found ->
            failwith (Printf.sprintf "Badly formatted optional field '%s'" str))
        lst
    in
    let is_urgency (k, _) = String.lowercase k = "urgency" in
    let _, urgency =
      try List.find is_urgency lst'
      with Not_found -> failwith "Cannot find urgency field"
    in
    (urgency, List.filter (fun e -> not (is_urgency e)) lst')
  in
  let rec parse_header line =
    try
      (* Try to parse header. *)
      let grps = Re.exec header_re line in
      let source = Re.Group.get grps 1 in
      let version = Re.Group.get grps 2 in
      let distribs = Re.Group.get grps 3 in
      let extra_fields = Re.Group.get grps 4 in
      let maintainer, timestamp =
        next "changes/trailer" parse_change_or_trailer
      in
      let urgency, optional_fields = parse_optional_fields extra_fields in
      let distributions = Re.(split (compile (rep1 space))) distribs in
      let changes = Buffer.contents buff in
      Buffer.clear buff;
      {
        source;
        version;
        distributions;
        optional_fields;
        urgency;
        maintainer;
        timestamp;
        changes;
      }
    with Not_found -> next "header" ~fst:line parse_header
  and parse_change_or_trailer line =
    if String.starts_with line "  " then (
      Buffer.add_substring buff line 2 (String.length line - 2);
      next "changes/trailer" parse_change_or_trailer )
    else
      try
        let grps = Re.exec trailer_re line in
        let maint = Re.Group.get grps 1 in
        let mail = Re.Group.get grps 2 in
        let timestamp = Re.Group.get grps 3 in
        (Printf.sprintf "%s <%s>" maint mail, timestamp)
      with Not_found ->
        next "changes/trailer" ~fst:line parse_change_or_trailer
  in
  parse_header fst

(** Only parse the first entry
  *)
let head ch =
  match skip_line ch with
  | _, Some line -> parse_one ch line
  | _, None -> failwith "No first changelog entry"

(** Parse the full changelog
  *)
let parse ch =
  let rec parse_aux lst =
    match skip_line ch with
    | _, Some line -> parse_aux (parse_one ch line :: lst)
    | _, None -> List.rev lst
  in
  parse_aux []

let to_string e =
  let buff = Buffer.create 13 in
  let add fmt =
    Printf.ksprintf
      (fun s ->
        Buffer.add_string buff s;
        Buffer.add_char buff '\n')
      fmt
  in
  add "%s (%s) %s; %s" e.source e.version
    (String.concat " " e.distributions)
    (String.concat ", "
       (List.map
          (fun (k, v) -> Printf.sprintf "%s=%s" k v)
          (("urgency", e.urgency) :: e.optional_fields)));
  add "";
  List.iter (add "  %s") (String.nsplit e.changes "\n");
  add "";
  add " -- %s %s" e.maintainer e.timestamp;
  Buffer.contents buff

let filename = Filename.concat "debian" "changelog"

let default () = with_fn filename parse

let default_head () = with_fn filename head
