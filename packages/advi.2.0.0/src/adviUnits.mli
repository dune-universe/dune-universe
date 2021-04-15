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

type units =
  | PT    (* point *)
  | PC    (* pica:              1 pc =    12 pt *)
  | IN    (* inch:              1 in = 72.27 pt *)
  | BP    (* big point         72 bp =     1 in *)
  | CM    (* centimeter:     2.54 cm =     1 in *)
  | MM    (* millimeter:       10 mm =     1 cm *)
  | DD    (* didot point:    1157 dd =  1238 pt *)
  | CC    (* cicero:            1 cc =    12 dd *)
  | SP ;; (* scaled point:  65536 sp =     1 pt *)

(* source: D. Knuth, The TeXbook, Addison-Wesley (C) 1986 *)

val ratio : units -> units -> int * int ;;
    (* [ratio u1 u2] returns a pair of integers [(n1, n2)]
       whose meaning is:  n1 u1 equals n2 u2.

       For example, [ratio IN CM] returns (50, 127), since
       50 inches = 127 centimeters, that is  1 in = 2.54 cm.

       The returned fraction n1/n2 is always irreducible. *)

val from_to : units -> units -> (float -> float) ;;
    (* [from_to u1 u2] returns a function for converting
       dimensions from u1 units to u2 units. *)
