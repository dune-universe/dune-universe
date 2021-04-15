(*
 * advi - A DVI previewer
 * Copyright (C) 2000  Alexandre Miquel
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * See the GNU Lesser General Public License version 2.1 for more
 * details (enclosed in the file LGPL).
 *)

open Format ;;
open Dvicommands;;

type page = {
    counters : int array ;
    commands : string;
  } ;;

type t = {
    preamble : preamble ;
    prelude : string ;
    pages : page array ;
    postamble : postamble ;
    font_map : (int * font_def) list
  } ;;

exception Error of string ;;

val load : string -> t ;;
  (** Load a dvi file and store all the command into the memory.
      Parsing of dvi commands is not performed. Whole the command text
      is splited into pages and stored as raw strings. *)
val parse_string : string -> command list ;;
  (** Parse a given dvi command string *)
val parse_page : page -> command list ;;
  (** Parse a dvi commands of a page *)
val string_iter : (command -> unit) -> string -> unit ;;
  (** Iteration over dvi commands obtained from a string by parsing *)
val page_iter : (command -> unit) -> page -> unit ;;
  (** Iteration over commands parsed from a dvi command string of a page *)
val page_step : (command -> unit) -> page -> (unit -> bool);;
  (** Make a stepping iterator over commands parsed from a dvi command 
     string of a page, if the iterator returns false, the page ends. *)

(* Printing dvi commands *)
val fprint_preamble : formatter -> preamble -> unit ;;
val fprint_postamble : formatter -> postamble -> unit ;;
val fprint_font_def : formatter -> font_def -> unit ;;
val fprint_command : formatter -> command -> unit ;;
