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
(*  Xavier Leroy, and Alan Schmitt.                                    *)
(*                                                                     *)
(*  Based on Mldvi by Alexandre Miquel.                                *)
(***********************************************************************)

(* $Id$ *)

type color = GraphicsY11.color;;

type x = GraphicsY11.x
and y = GraphicsY11.y
and w = GraphicsY11.w
and h = GraphicsY11.h;;

module Make
  (G : sig
     type t
     val hoffset : t -> int
     val voffset : t -> int
     val width : t -> int
     val height : t -> int
   end) : sig

  type glyph = G.t;;
  type fontname = string;;
  type fontratio = float;;

  type g = {
    fontname : string;
    fontratio : float;
    glyph : glyph;
  };;

  type symbol =
     | Glyph of g
     | Space of w * h
     | Rule of w * h
     | Line of Misc.line_number * Misc.file_name option;;

  type code = int;;

  type display_symbol = {
    color : color;
    locx : x;
    locy : y;
    code : code;
    symbol : symbol;
  };;

  type display_set = display_symbol list;;

  val voffset : display_symbol -> y;;
  val hoffset : display_symbol -> x;;
  val height : display_symbol -> h;;
  val width : display_symbol -> w;;

  val clear_global_display_set : unit -> unit;;
  val add_to_global_display_set : color -> x -> y -> code -> symbol -> unit;;
  (** color -> locx -> locy -> code -> symbol -> unit *)

  val to_ascii : display_set -> string;;
  (** [to_ascii ds] returns a string representing the display symbols that
      are in the display_set. *)
  (* Could be done with a pretty printer... *)
  val to_ascii_escaped : display_set -> string;;

  val commands_to_ascii :
    (int * Dvicommands.font_def) list -> Dvicommands.command list -> string

  val inzone : x -> y -> x -> y -> display_set;;
  (** Gives a copy of the global [display_set] where only symbols inside
     the zone x1 y1 x2 y2 are kept. *)

  val intime : x -> y -> x -> y -> display_set;;
  (** Idem where but the resulting display_set is time-convex
     (intermediate symbols are also kept). *)

  val iter : (display_symbol -> unit) -> display_set -> unit;;
  (** [iter ff ds] Iterates the function [ff] over the [display_set]
      [ds] set of display symbols. *)

  (** Regions. *)

  type region;;

  val position : int -> int -> region;;
  val new_region : region -> int -> int -> region;;
  val iter_region : (display_symbol -> unit) -> region -> unit;;
  val iter_regions :
      (display_symbol -> unit) -> (display_symbol -> unit) ->
      region -> region -> unit;;
  val apply : (glyph -> int -> int -> int -> unit) -> display_symbol -> unit;;

  val lines : int -> int ->
    (display_symbol * int * int *
     string * string * string * string * string option) option;;
  val word : int -> int -> (region * string) option;;
  val region_to_ascii : region -> string;;

end;;
