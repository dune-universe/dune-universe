(*
 * Copyright (c) 2012-2015 Vincent Bernardoff <vb@luminar.eu.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

%{
open Sgftypes

let text_of_string str = Text str

let point_of_string str = Point (str.[0], str.[1])

let move_of_string str =
  try Move (Some (str.[0], str.[1]))
  with Invalid_argument _ -> Move None

let double_of_string = function
  | "1" -> Normal
  | "2" -> Emph
  | _   -> failwith "double_of_string"

let color_of_string = function
  | "B" | "b" -> Black
  | "W" | "w" -> White
  | s -> failwith (Printf.sprintf "color_of_string: got %s" s)

let number_of_string ?range str =
  let number = int_of_string str in match range with
    | Some (a,b) -> assert (number >= a && number <= b); Number number
    | None -> Number number

let real_of_string str = Real (float_of_string str)

let compose_of_string ~f ?s str =
  let open String in
  try
    let len = length str in
    let i = index str ':' in
    let a = sub str 0 i in
    let b = sub str (i+1) (len-i-1) in
    (match s with
     | None -> Compose (f a, f b)
     | Some s -> Compose (f a, s b))
  with Not_found -> invalid_arg "compose_of_string"

let property_of_tuple pname pvalues =
  let pvalue = try List.hd pvalues with Failure _ -> "" in
  match pname with
    (* Move *)
    | "B" | "W" -> pname, List (List.map move_of_string pvalues)
    | "KO" -> pname, One Empty
    | "MN" -> pname, One (number_of_string pvalue)

    (* Setup *)
    | "AB" | "AE" | "AW" -> pname, List (List.map point_of_string pvalues)
    | "PL" -> pname, One (color_of_string pvalue)

    (* Node annotation *)
    | "C" | "N" -> pname, One (Text pvalue)
    | "DM" | "GB" | "GW" | "HO" | "UC" -> pname, One (double_of_string pvalue)
    | "V"  -> pname, One (real_of_string pvalue)

    (* Move annotation *)
    | "BM" | "TE" -> pname, One (double_of_string pvalue)
    | "DO" | "IT" -> pname, One Empty

    (* Markup *)
    | "AR" | "LN" -> pname, List (List.map (compose_of_string ~f:point_of_string) pvalues)
    | "CR" | "DD" -> pname, List (List.map point_of_string pvalues)
    | "LB" -> pname, List (List.map (compose_of_string ~f:point_of_string ~s:text_of_string) pvalues)
    | "MA" | "SL" | "SQ" | "TR" -> pname, List (List.map point_of_string pvalues)

    (* Root *)
    | "AP" -> pname, One (compose_of_string ~f:text_of_string pvalue)
    | "CA" -> pname, One (Text pvalue)
    | "FF" -> pname, One (number_of_string ~range:(1,4) pvalue)
    | "GM" -> pname, One (number_of_string ~range:(1,16) pvalue)
    | "ST" -> pname, One (number_of_string ~range:(0,3) pvalue)
    | "SZ" -> pname,
      (try One (compose_of_string ~f:number_of_string pvalue)
       with Invalid_argument _ -> One (number_of_string pvalue))

    (* Game Info *)
    | "AN" | "BR" | "BT" | "CP" | "DT" | "EV" | "GN" | "GC" | "ON"
    | "OT" | "PB" | "PC" | "PW" | "RE" | "RO" | "RU" | "SO" | "US"
    | "WR" | "WT" -> pname, One (Text pvalue)
    | "TM" -> pname, One (real_of_string pvalue)

    (* Timing *)
    | "BL" | "WL" -> pname, One (real_of_string pvalue)
    | "OB" | "OW" -> pname, One (number_of_string pvalue)

    (* Go specific *)
    | "HA" -> pname, One (number_of_string pvalue)
    | "KM" -> pname, One (real_of_string pvalue)
    | "TB" | "TW" -> pname, List (List.map point_of_string pvalues)

    (* Misc *)
    | "FG" -> pname, (match pvalues with [] -> One Empty | _ ->
      List (List.map (compose_of_string ~f:point_of_string ~s:text_of_string) pvalues))
    | "PM" -> pname, One (number_of_string pvalue)
    | "VW" -> pname, List (List.map point_of_string pvalues)

    (* Unknown *)
    | oth -> oth, One (Text pvalue)
%}

%token<string> PROPNAME PROPCONTENT
%token LPAR RPAR SEMI EOF

%start <Sgftypes.collection> collection

%%

collection:
| col = gametree+ EOF { (col:collection) }

gametree:
| LPAR seq = sequence RPAR { Leaf seq }
| LPAR seq = sequence gt = gametree+ RPAR { Node (seq, gt) }

sequence:
| seq = node+ { (seq:sequence) }

node:
| SEMI pl = property+ { (pl:node) }

property:
| name = PROPNAME vl = PROPCONTENT+ { property_of_tuple name vl }
