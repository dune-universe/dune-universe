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

open Rresult

include Sgftypes

type err =
  | Lexing_error of Lexing.position * string
  | Parsing_error of Lexing.position

let rec string_of_pvalue = function
  | Empty -> ""
  | Number n -> Printf.sprintf "%d" n
  | Real r  -> Printf.sprintf "%.2f" r
  | Normal -> "1" | Emph -> "2"
  | Black -> "B" | White -> "W"
  | Text str -> Printf.sprintf "%s" str
  | Point (a,b) -> Printf.sprintf "%c%c" a b
  | Move (Some (a,b)) -> Printf.sprintf "%c%c" a b
  | Move None -> ""
  | Compose (a,b) -> string_of_pvalue a ^ ":" ^ string_of_pvalue b

let pp_property fmt (pname, pvalues) =
  Format.fprintf fmt "%s" pname;
  (match pvalues with
   | One pvalue -> Format.fprintf fmt "[%s]" (string_of_pvalue pvalue)
   | List pvalues -> List.iter
                       (fun pv -> Format.fprintf fmt "[%s]" (string_of_pvalue pv)) pvalues)

let pp_node fmt n =
  Format.fprintf fmt ";";
  List.iter (fun x -> pp_property fmt x) n

let pp_sequence fmt s =
  List.iter (fun x -> pp_node fmt x) s

let rec pp_gametree fmt gm =
  Format.fprintf fmt "(";
  (match gm with
   | Node (seq, gts)  ->
     pp_sequence fmt seq; List.iter (fun x -> pp_gametree fmt x) gts
   | Leaf seq ->
     pp_sequence fmt seq);
  Format.fprintf fmt ")"

let pp_collection fmt c =
  List.iter (fun x -> pp_gametree fmt x; Format.fprintf fmt "\n") c

let parse fname menhir_parser lexbuf =
  let open Lexing in
  let position = ref
      { pos_fname = fname; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 } in
  let lexer () =
    let ante_position = !position in
    let nlines, token = Lexer.main_scanner 1 lexbuf in
    position := {!position with pos_lnum = !position.pos_lnum + nlines;};
    let post_position = !position
    in (token, ante_position, post_position) in
  let revised_parser =
    MenhirLib.Convert.Simplified.traditional2revised menhir_parser
  in
  try
    R.ok @@ revised_parser lexer
  with
  | Failure x -> R.error @@ Lexing_error (!position, x)
  | Parser.Error -> R.error @@ Parsing_error !position

let of_string s =
  let lexbuf = Sedlexing.Utf8.from_string s in
  parse "" Parser.collection lexbuf

let of_channel ic =
  let lexbuf = Sedlexing.Utf8.from_channel ic in
  parse "" Parser.collection lexbuf

let of_file fn =
  let ic = open_in fn in
  try
    let r = of_channel ic in close_in ic; r
  with exn ->
    close_in ic; raise exn
