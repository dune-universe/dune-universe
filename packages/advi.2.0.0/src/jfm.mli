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

type preamble;;
type header;;

type char_info = {
  width_index : int;
  height_index : int;
  depth_index : int;
  italic_index : int;
  tag : int;
  remainder : int;
};;

type jfm = {
  preamble : preamble;
  header : header;
  char_types : (int, int) Hashtbl.t;
  char_infos : char_info array;
  widths : int array;
  heights : int array;
  depths : int array;
  italics : int array;
  gluekerns : string;
  glues : int array;
  kerns : int array;
  params : string;
};;

val load_jfm_file : string -> jfm;;
val find_width : jfm -> int -> int;;

val monospace_fix : (int * float) list;;
