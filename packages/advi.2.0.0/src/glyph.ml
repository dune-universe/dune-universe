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

type t = {
    width : int ;
    height : int ;
    voffset : int ;
    hoffset : int ;
    graymap : string
  } ;;

let make_table len off ratio =
  let left = off
  and right = len - off in
  let left' = int_of_float (ceil (ratio *. float left))
  and right' = int_of_float (ceil (ratio *. float right)) in
  let len' = left' + right'
  and off' = left' in
  if len' = 0 then
    (len', off', [| |])
  else begin
    let table = Array.make len' (0, 0.0, 0, 0.0)
    and inv = 1.0/.ratio in
    for i' = 0 to len' - 1 do
      let lo = float(i' - off')/.ratio +. float off in
      let hi = lo +. inv in
      let fi0 = floor lo
      and fi1 = ceil hi -. 1.0 in
      let i0 = int_of_float fi0
      and i1 = int_of_float fi1 in
      table.(i') <-
	if i0 = i1 then
	  (i0, inv, i1, inv)
	else
	  (i0, fi0 +. 1.0 -. lo, i1, hi -. fi1)
    done ;
    (len', off', table)
  end ;;

let from_char_def cdef ratio =
  let bitmap = cdef.Font.bitmap
  and ncols = cdef.Font.width
  and nrows = cdef.Font.height
  and hot_col = cdef.Font.hoffset
  and hot_row = cdef.Font.voffset in
  let (ncols', hot_col', col_table) = make_table ncols hot_col ratio
  and (nrows', hot_row', row_table) = make_table nrows hot_row ratio in
  let graymap = Bytes.create (ncols' * nrows')
  and index = ref 0
  and fact = 256.0 *. ratio *. ratio
  and last_col = ncols - 1
  and last_row = nrows - 1 in
  for i' = 0 to nrows' - 1 do
    let (i0, c0, i1, c1) = row_table.(i') in
    for j' = 0 to ncols' - 1 do
      let sum = ref 0.0
      and (j0, d0, j1, d1) = col_table.(j') in
      for i = max 0 i0 to min i1 last_row do
	let base = i * ncols in
	for j = max 0 j0 to min j1 last_col do
	  let pos = base + j in
	  if Char.code bitmap.[pos lsr 3] land
	    (0x80 lsr (pos land 7)) <> 0 then
	    if i = i0 then
	      if j = j0 then sum := !sum +. c0 *. d0 else
	      if j = j1 then sum := !sum +. c0 *. d1 else
	      sum := !sum +. c0
	    else if i = i1 then
	      if j = j0 then sum := !sum +. c1 *. d0 else
	      if j = j1 then sum := !sum +. c1 *. d1 else
	      sum := !sum +. c1
	    else
	      if j = j0 then sum := !sum +. d0 else
	      if j = j1 then sum := !sum +. d1 else
	      sum := !sum +. 1.0
	done
      done ;
      let gray = Misc.round (!sum *. fact) in
      Bytes.set graymap !index (Char.chr (max 0 (min gray 255))) ;
      incr index
    done
  done ;
  { width = ncols' ;
    height = nrows' ;
    hoffset = hot_col' ;
    voffset = hot_row' ;
    graymap = Bytes.to_string graymap } ;;
