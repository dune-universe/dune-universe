(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            François Pessaux, projet Cristal, INRIA Rocquencourt     *)
(*            Pierre Weis, projet Cristal, INRIA Rocquencourt          *)
(*            Jun Furuse, projet Cristal, INRIA Rocquencourt           *)
(*                                                                     *)
(*  Copyright 1999-2004,                                               *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: mstring.ml,v 1.1 2006/11/28 15:43:28 rousse Exp $*)

(** String utilities *)

open Util

(** split a string according to char_sep predicate *)
let split_str char_sep str =
  let len = String.length str in
  if len = 0 then [] else
    let rec skip_sep cur =
      if cur >= len then cur else
      if char_sep str.[cur] then skip_sep (succ cur)
      else cur  in
    let rec split beg cur =
      if cur >= len then
        if beg = cur then []
        else [String.sub str beg (len - beg)] else
      if char_sep str.[cur] then
        let nextw = skip_sep cur in
        String.sub str beg (cur - beg) :: split nextw nextw
      else split beg (succ cur) in
    let wstart = skip_sep 0 in
    split wstart wstart

(* split a string according to char_sep predicate *)
let split_str_quoted char_sep str =
  let len = String.length str in
  if len = 0 then [] else
  let cword = ref "" in
    let rec skip_sep cur =
      if cur >= len then cur else
      if char_sep str.[cur] then skip_sep (succ cur)
      else cur in
    let rec close_quote cur =
      if cur >= len then cur else
      if str.[cur] = '"' then cur else begin
        cword := !cword ^ String.make 1 str.[cur];
        close_quote (succ cur)
      end in
  let rec split beg cur =
    if cur >= len then
      if beg = cur then [] else [!cword] else
    if str.[cur] = '"' then
      let endquote = close_quote (succ cur) in
      split beg (succ endquote) else
    if char_sep str.[cur] then
      let nextw = skip_sep cur in
      let word = !cword in
      cword := "";
      word :: split nextw nextw
    else begin
      cword := !cword ^ String.make 1 str.[cur];
      split beg (succ cur)
    end in

  let wstart = skip_sep 0 in
  split wstart wstart

(* extract the . suffix (dot excluded) of a string *)
let get_suffix s =
  try
    let dotpos = succ (String.rindex s '.') in
    String.sub s dotpos (String.length s - dotpos)
  with
  | Not_found -> ""

(* HEX/DEC conversions *)
let hex_to_dec c = match c with
  | '0'..'9' -> int_of_char c - 48
  | 'a'..'f' -> int_of_char c - 87 (* 87 = int_of_char 'a' - 10 *)
  | 'A'..'F' -> int_of_char c - 55 (* 55 = int_of_char 'A' - 10 *)
  | _ -> failwith "hex_to_dec"

let dec_to_hex i =
  if i < 10 then char_of_int (i + 48)  (* 48 = int_of_char '0' *)
  else char_of_int (i + 55)            (* 55 = int_of_char 'A' - 10 *)

(* Converting a hex stored string *)
let hex_to_string s =
  let len = String.length s / 2 in
  String.init len @@ fun i -> 
    char_of_int ( 16 * (hex_to_dec s.[i + i]) + hex_to_dec s.[i + i + 1])


let gensym =
  let cnter = ref 0 in
  (fun n ->
    incr cnter;
    n ^ string_of_int !cnter)

let rem_trailing_sp s =
  let l = String.length s in
  let pos = ref (l - 1) in
  while !pos >= 0 && List.mem s.[!pos] [' '; '\t'] do decr pos done;
  if !pos = l - 1 then s
  else String.sub s 0 (succ !pos)

let catenate_sep = String.concat

(** Filters CRLF:
 -  CR -> LF
 -  CRLF -> LF
 -  LF -> LF
 We do this on successive chunks of a stream, so we need to consider
 the case when the chunk finishes on CR.
 Assume len > 0
*)

let norm_crlf lastwascr buf offs len =
  let rpos = ref offs
  and wpos = ref 0
  and dest = Bytes.create (len + 1) (* we need one more char *)
  and limit = offs + len - 1
  and lastiscr = ref false in
  if lastwascr then
    if buf.[!rpos] = '\n' then begin
      dest << !wpos & '\n';
      incr rpos; incr wpos
    end else begin
      dest << !wpos & '\n'; incr wpos
    end;

  while !rpos < limit do
    match buf.[!rpos] with
    | '\n' -> dest << !wpos & '\n'; incr rpos; incr wpos
    | '\r' ->
      if buf.[!rpos + 1] = '\n' then begin
        dest << !wpos & '\n'; rpos := !rpos + 2; incr wpos
      end else begin
        dest << !wpos & '\n'; incr rpos; incr wpos end
    | c -> dest << !wpos & c; incr rpos; incr wpos
  done;
  begin match buf.[offs+len-1] with
  | '\n' -> dest << !wpos & '\n'; incr wpos
  | '\r' -> lastiscr := true
  | c -> dest << !wpos & c; incr wpos
  end;
  Bytes.sub_string dest 0 !wpos, !lastiscr

let hexchar c =
  let i = int_of_char c in
  String.init 3 @@ function
    | 0 -> '%'
    | 1 -> dec_to_hex (i / 16)
    | 2 -> dec_to_hex (i mod 16)
    | _ -> assert false
