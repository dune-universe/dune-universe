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
(*  Xavier Leroy, Clément Renard, and Alan Schmitt.                    *)
(*                                                                     *)
(*  Based on Mldvi by Alexandre Miquel.                                *)
(***********************************************************************)

(* $Id$ *)

type offset =
   | No_offset
   | Plus of int
   | Minus of int;;

let int_of_offset = function
  | No_offset -> 0
  | Plus n -> n
  | Minus n -> -n;;

type t = {
    mutable width : int;
    mutable height : int;
    mutable xoffset : offset;
    mutable yoffset : offset
};;

let parse s =
  try
    let len = String.length s
    and i = ref 0 in
    let parse_int () =
      if !i = len || not (Misc.is_digit s.[!i] || s.[!i] == '-') then
        invalid_arg "Geometry.parse";
      let start = !i in
      if s.[!i] = '-' && !i < len+1 then incr i;
      while !i < len && Misc.is_digit s.[!i] do incr i done;
      let stop = !i in
      int_of_string (String.sub s start (stop - start)) in
    let parse_offset () =
      if !i = len || (s.[!i] <> '+' && s.[!i] <> '-') then
        No_offset
      else begin
        let sgn = s.[!i] in
        incr i;
        if !i = len || not (Misc.is_digit s.[!i] || s.[!i] == '-') then
          No_offset
        else
          match sgn with
          | '+' -> Plus (parse_int ())
          | '-' -> Minus (parse_int ())
          | _ -> assert false
      end in
    while !i < len && s.[!i] = ' ' do incr i done;
    let width = parse_int () in
    if !i = len || (s.[!i] <> 'x' && s.[!i] <> 'X') then
      invalid_arg "Geometry.parse";
    incr i;
    let height = parse_int () in
    let xoffset = parse_offset () in
    let yoffset = parse_offset () in
    { width = width; height = height; xoffset = xoffset; yoffset = yoffset }
    with
    | Failure _ -> invalid_arg "Geometry.parse";;

let to_string g =
  let w = g.width
  and h = g.height
  and xoff = g.xoffset
  and yoff = g.yoffset in
  match (xoff, yoff) with
  | (No_offset, No_offset) -> Printf.sprintf "%dx%d" w h
  | (Plus x, No_offset) -> Printf.sprintf "%dx%d+%d" w h x
  | (Minus x, No_offset) -> Printf.sprintf "%dx%d-%d" w h x
  | (No_offset, Plus y) -> Printf.sprintf "%dx%d++%d" w h y
  | (No_offset, Minus y) -> Printf.sprintf "%dx%d+-%d" w h y
  | (Plus x, Plus y) -> Printf.sprintf "%dx%d+%d+%d" w h x y
  | (Plus x, Minus y) -> Printf.sprintf "%dx%d+%d-%d" w h x y
  | (Minus x, Plus y) -> Printf.sprintf "%dx%d-%d+%d" w h x y
  | (Minus x, Minus y) -> Printf.sprintf "%dx%d-%d-%d" w h x y;;




