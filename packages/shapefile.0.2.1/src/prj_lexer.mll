(* Copyright 2018 Cyril Allignol
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations
 * under the License. *)

{ open Prj_parser
  open Prj_syntax
let keyword =
  let keywords = Hashtbl.create 17 in
  let kwds =
    [ "PARAMETER", PARAMETER; "PARAM_MT", PARAM_MT; "CONCAT_MT", CONCAT_MT;
      "INVERSE_MT", INVERSE_MT; "PASSTHROUGH_MT", PASSTHROUGH_MT;
      "NORTH", DIRECTION Axis.North; "SOUTH", DIRECTION Axis.South;
      "EAST", DIRECTION Axis.East; "WEST", DIRECTION Axis.West;
      "UP", DIRECTION Axis.Up; "DOWN", DIRECTION Axis.Down;
      "OTHER", DIRECTION Axis.Other;
      "UNIT", UNIT; "AUTHORITY", AUTHORITY; "AXIS", AXIS; "TOWGS84", TOWGS84;
      "PROJECTION", PROJECTION; "SPHEROID", SPHEROID; "PRIMEM", PRIMEM;
      "DATUM", DATUM; "VERT_DATUM", VERT_DATUM; "LOCAL_DATUM", LOCAL_DATUM;
      "PROJCS", PROJCS; "GEOGCS", GEOGCS; "GEOCCS", GEOCCS; "VERT_CS", VERT_CS;
      "COMPD_CS", COMPD_CS; "FITTED_CS", FITTED_CS; "LOCAL_CS", LOCAL_CS ] in
  List.iter (fun (kwd, token) -> Hashtbl.add keywords kwd token) kwds;
  fun word ->
    if Hashtbl.mem keywords word then Hashtbl.find keywords word
    else failwith ("Unknown keyword: " ^ word)
}

let blank = [' ''\t''\r''\n'',']
let digit = ['0'-'9']
let nb = ['-']? ((digit+ (['.']? digit*)) | (['.'] digit+))
let str = '\"' (_ # '\"')* '\"'
let kwd = ['A'-'Z']['A'-'Z''0'-'9''_']*

rule token = parse
| blank    { token lexbuf }
| ['(''['] { LP }
| [')'']'] { RP }
| kwd as k { keyword k }
| str as s { STRING (String.sub s 1 (String.length s - 2)) }
| nb as n  { N (float_of_string n) }
| eof      { EOF }
