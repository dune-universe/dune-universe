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

type dimen =
   | Px of int
   | Pt of float
   | Pc of float
   | In of float
   | Bp of float
   | Cm of float
   | Mm of float
   | Dd of float
   | Cc of float
   | Sp of int;;

let is_digit c = c >= '0' && c <= '9';;

let dimen_of_string str =
  let len = String.length str in
  let i = ref 0 in
  if !i < len && str.[!i] = '-' then incr i;
  while !i < len && is_digit str.[!i] do incr i done ;
  if !i < len && str.[!i] = '.' then begin
    incr i ;
    while !i < len && is_digit str.[!i] do incr i done
  end ;
  let (pref, suff) =
    (String.sub str 0 !i, String.sub str !i (len - !i)) in
  let f = float_of_string pref in
  match suff with
  | "" -> Px (Misc.round f)
  | "pt" -> Pt f
  | "pc" -> Pc f
  | "in" -> In f
  | "bp" -> Bp f
  | "cm" -> Cm f
  | "mm" -> Mm f
  | "dd" -> Dd f
  | "cc" -> Cc f
  | "sp" -> Sp (Misc.round f)
  | _ -> invalid_arg (Printf.sprintf "unknown unit `%s'" suff);;

let normalize = function
  | Px n -> Px n
  | Pt f -> In (AdviUnits.from_to AdviUnits.PT AdviUnits.IN f)
  | Pc f -> In (AdviUnits.from_to AdviUnits.PC AdviUnits.IN f)
  | In f -> In f
  | Bp f -> In (AdviUnits.from_to AdviUnits.BP AdviUnits.IN f)
  | Cm f -> In (AdviUnits.from_to AdviUnits.CM AdviUnits.IN f)
  | Mm f -> In (AdviUnits.from_to AdviUnits.MM AdviUnits.IN f)
  | Dd f -> In (AdviUnits.from_to AdviUnits.DD AdviUnits.IN f)
  | Cc f -> In (AdviUnits.from_to AdviUnits.CC AdviUnits.IN f)
  | Sp n -> In (AdviUnits.from_to AdviUnits.SP AdviUnits.IN (float n));;

