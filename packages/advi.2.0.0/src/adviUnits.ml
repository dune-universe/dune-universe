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
  | PT
  | PC
  | IN
  | BP
  | CM
  | MM
  | DD
  | CC
  | SP ;;

(*** Rational arithmetic ***)

let rec unsafe_gcd big small =
  if small = 0 then big else
  unsafe_gcd small (big mod small) ;;

let gcd a b =
  if a > b then unsafe_gcd a b else
  if a < b then unsafe_gcd b a else a ;;

let simplify (num, den) =
  let d = gcd num den in
  (num/d, den/d) ;;

let rec mag n =
  if n = 0 then 0 else
  1 + mag (n/2) ;;

let mul (n1, d1) (n2, d2) =
  simplify (n1 * n2, d1 * d2) ;;

let div r (n, d) =
  mul r (d, n) ;;

let conv_list = ref [PT, (1, 1)] ;;

let set k1 u1 (* = *) k2 u2 =
  assert (not (List.mem_assoc u1 !conv_list)) ;
  let r2 = List.assoc u2 !conv_list in
  let r1 = mul r2 (k2, k1) in
  conv_list := (u1, r1) :: !conv_list ;;

(* Convertion ratios (source: D. Knuth, The TeXbook 1986, Addisson-Wesley) *)

set 1 PC (* = *) 12 PT ;;
set 100 IN (* = *) 7227 PT ;;
set 72 BP (* = *) 1 IN ;;
set 254 CM (* = *) 100 IN ;;
set 10 MM (* = *) 1 CM ;;
set 1157 DD (* = *) 1238 PT ;;
set 1 CC (* = *) 12 DD ;;
set 65536 SP (* = *) 1 PT ;;

let int_of_units = function
  | PT -> 0 | PC -> 1 | IN -> 2
  | BP -> 3 | CM -> 4 | MM -> 5
  | DD -> 6 | CC -> 7 | SP -> 8 ;;

let units_of_int = function
  | 0 -> PT | 1 -> PC | 2 -> IN
  | 3 -> BP | 4 -> CM | 5 -> MM
  | 6 -> DD | 7 -> CC | 8 -> SP
  | _ -> assert false ;;

let matrix = Array.make_matrix 9 9 (0, 0) ;;

for i = 0 to 8 do
  for j = 0 to 8 do
    let ri = List.assoc (units_of_int i) !conv_list
    and rj = List.assoc (units_of_int j) !conv_list in
    matrix.(i).(j) <- div rj ri
  done
done ;;

let ratio u1 u2 =
  matrix.(int_of_units u1).(int_of_units u2) ;;

let from_to u1 u2 =
  let (n1, n2) = ratio u1 u2 in
  let f = float n2 /. float n1 in
  function x -> f *. x ;;
