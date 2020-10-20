(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Jun Furuse, projet Cristal, INRIA Rocquencourt           *)
(*                                                                     *)
(*  Copyright 1999-2004,                                               *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: enhance.ml,v 1.5 2004/09/24 10:55:06 weis Exp $ *)

open Images
open Colorhist

let make_id_rgb rgb = rgb

let enhance keep t =
  let total = total_samples t in
  if total = 0 then raise (Failure "histgram is empty");
  let cut_samples = truncate ((float total) *. (1.0 -. keep) /. 2.0) in

  let rec find_limit update sum b =
    let sum = sum + t.(b) in
    if sum > cut_samples then b
    else find_limit update sum (update b) in

  let min = find_limit ((+) 1) 0 0
  and max = find_limit ((+) (-1)) 0 255 in

  if max <= min then 1.0, make_id_rgb else begin
    let average =
      let samples = ref 0 in
      let sum = ref 0 in
      for i = min to max do
        sum := !sum + t.(i) * (i - min);
        samples := !samples + t.(i)
      done;
      float !sum /. float !samples /. (float (max - min)) in

    let logmax = 1.0 /. 0.5 in
    let logmin = 1.0 /. 2.0 in

    (* average will go near 0.65 *)
    let log = log 0.65 /. log average in
    let log =
      if logmax < log then logmax else
      if logmin > log then logmin else log in

    prerr_endline
      (Printf.sprintf
         "average=%f gamma=%f min=%d max=%d"
         (average *. 255.0) (1.0 /. log) min max);

    let div = 2 in

    let make_elem x =
      if x < min then x / div else
      if x > max then 255 - (255 - x) / div else
      let x' =
        if x - min < (max - min) / 2
        then x / div
        else 255 - (255 - x) / div in
      let pow =
        float (abs (x - min - (max - min) / 2)) /. float ((max - min) / 2) in
      let pow = pow *. pow in
      let len = float (max - min) in
      let newlen = float (255 - min / div - (255 - max) / div) in
      let x'' =
        truncate (((float (x - min) /. len) ** log) *. newlen) + min / div in
      int_of_float (float x' *. pow +. float x'' *. (1.0 -. pow) ) in
    let table = Array.init 256 make_elem in

    let make_table_rgb rgb = {
        r = table.(rgb.r);
        g = table.(rgb.g);
        b = table.(rgb.b);
    } in

    log, make_table_rgb
  end

