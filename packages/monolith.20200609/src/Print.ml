(******************************************************************************)
(*                                                                            *)
(*                                  Monolith                                  *)
(*                                                                            *)
(*                              François Pottier                              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU Lesser General Public License as published by the Free   *)
(*  Software Foundation, either version 3 of the License, or (at your         *)
(*  option) any later version, as described in the file LICENSE.              *)
(*                                                                            *)
(******************************************************************************)

include PPrint

type 'a printer =
  'a -> document

let block doc =
  nest 2 (break 0 ^^ doc) ^^ break 0

let parens doc =
  lparen ^^ block doc ^^ rparen

let raw_apply docs =
  group (flow (break 1) docs)

let apply doc docs =
  raw_apply (doc :: docs)

let toplevel_let pat body =
  group (
    utf8string "let " ^^ pat ^^ utf8string " =" ^^
    nest 2 (break 1 ^^ body) ^^ utf8string ";;"
  )

let output doc =
  ToChannel.pretty 0.9 78 stdout (group doc)

let unit =
  string "()"

let bool =
  OCaml.bool

let char =
  OCaml.char

let int i =
  utf8format (if i < 0 then "(%d)" else "%d") i

let option f = function
  | None ->
      string "None"
  | Some x ->
      parens (string "Some " ^^ f x)

let pair f1 f2 (x1, x2) =
  OCaml.tuple [ f1 x1; f2 x2 ]

let list =
  OCaml.flowing_list

let array =
  OCaml.flowing_array

let candidate_finds doc =
  group (
    break 1 ^^
    string "(* candidate finds " ^^
    doc ^^
    string " *)"
  )

let assert_ doc =
  apply (string "assert") [ parens doc ]
