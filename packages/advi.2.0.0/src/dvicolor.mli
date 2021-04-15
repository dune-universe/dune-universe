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

val parse_color : string -> color;;
(** Parses a string and return a color.
    A valid input string can be:
    - a color defined by [dvips], such as ["YellowOrange"] or ["Melon"],
    - a color predefined in the [Graphics] module, such as ["yellow"],
    - a color know by the [Camlimage] library (including X colors),
    - a mere integer specification (as understood by the Caml
      [int_of_string] primitive.
    Raises [Failure "int_of_string"] if the given string is not understood.
*)

val parse_color_args : string list -> color;;
(** Parses a list of strings as a color.
    The list may be:
    - a singleton [s]: [s] is parsed using [parse_color].
    - an empty list maps to medium gray (cmyk (0, 0, 0, 1/2).
    - otherwise, the first string of the list governs its
    interpretation; if it is:
    - ["rgb"], then the list next 3 arguments are used to specify the
      red, green, and blue components of the color,
    - ["cmyk"], then the list next 4 arguments are used to specify the
      cyan, magenta, yellow, and black components of the color,
    - ["gray"] or ["grey"], then the list next argument is used to
      specify the amount of gray of the color.
    All the color component specifications are supposed to be floating
    point numbers string representation as understood by [float_of_string].

    [parse_color_args] never fails. In case something goes wrong
    during parsing, a medium gray (cmyk (0, 0, 0, 1/2) is returned.
*)
