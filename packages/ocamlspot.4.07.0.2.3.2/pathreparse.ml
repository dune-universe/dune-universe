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

open Utils
open Ext

open Spot

open Path

open Locident

(* path substring *)

(* [get_substring mlpath pos r path]
   [mlpath] : source code
   [region] : the region 
   [pos] : cursor position
   [path] : the path of the whole region
*)
let get mlpath region pos path = 
  try
    let region = Region.complete mlpath region in
    let str = snd (Region.substring mlpath region) in
    try
      let pos = Position.complete mlpath pos in
      let pos = 
        match pos.Position.bytes, region.Region.start.Position.bytes with 
        | Some pos_bytes, Some start_bytes -> pos_bytes - start_bytes
        | _ -> failwith "The given position is not clear enough"
      in
      
      let lexbuf = Lexing.from_string str in
      let locid = Parser.locident Lexer.token lexbuf in
      let loc_in locid =
        locid.lident_loc.Location.loc_start.Lexing.pos_cnum <= pos 
        && pos < locid.lident_loc.Location.loc_end.Lexing.pos_cnum
      in
      let position_add pos_start diff =
        { Position.line_column = 
  	  Option.map pos_start.Position.line_column ~f:(fun (line,col) ->
  	    if diff.Lexing.pos_lnum = 1 then (* no new line *)
  	      (line, col + diff.Lexing.pos_cnum)
  	    else
  	      (line + diff.Lexing.pos_lnum - 1, diff.Lexing.pos_cnum));
          bytes = 
  	  Option.map pos_start.Position.bytes ~f:(fun bytes ->
  	    bytes + diff.Lexing.pos_cnum ) }
      in
      let subregion locid = Region.change_positions region 
        (position_add region.Region.start locid.lident_loc.Location.loc_start)
        (position_add region.Region.start locid.lident_loc.Location.loc_end)
      in
      let search path locid = 
        (* The last id name can be different. For example,
  	   module M = struct type t = Foo end
  	   let x = M.Foo
  	 M.Foo is not recorded as a use of M.Foo for now but as a use of M.t.
        *)
        let rec search ignore_suffix_diff path locid =
  	match path, locid.lident_desc with
  	| Pident _, LLident _ -> path, locid
  	| Pdot (path', pname, _), LLdot (locid', lname) ->
  	    if ignore_suffix_diff || pname = lname then
   	      if loc_in locid' then search false path' locid'
  	      else path, locid
  	    else failwith "mismatch"
  	| Papply (path1, path2), LLapply (locid1, locid2) ->
   	    if loc_in locid1 then search false path1 locid1
   	    else if loc_in locid2 then search false path2 locid2
  	    else path, locid
  	| Pdot (_, pname, _), LLident lname -> 
  	    if pname = lname then path, locid
  	    else failwith "mismatch"
  	| _ -> assert false
        in
        try search true path locid with
        | Failure s -> 
  	  Format.eprintf "Error: pathreparse: %s (path) <> %a (from source)@."
  	    (Path.name path) 
  	    Locident.format locid;
  	  failwith s
      in
      if loc_in locid then 
        let path, locid = search path locid in
        Some (path, subregion locid)
      else None
    with
    | e -> 
        Format.printf  "Pathreparse: not supported: %s (%s)@." 
  	str
  	(Printexc.to_string e);
        None;
  with
  | e ->
        Format.printf  "Pathreparse: not supported: (%s)@." 
  	(Printexc.to_string e);
        None;
;;
