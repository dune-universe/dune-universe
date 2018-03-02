(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            François Pessaux, projet Cristal, INRIA Rocquencourt     *)
(*            Pierre Weis, projet Cristal, INRIA Rocquencourt          *)
(*            Jun Furuse, projet Cristal, INRIA Rocquencourt           *)
(*                                                                     *)
(*  Copyright 1999-2004,                                               *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: colorhist.ml,v 1.1 2006/11/28 15:43:28 rousse Exp $ *)

open Color

type t = int array

let create () = Array.make 256 0

let total_samples t = Array.fold_left (fun st x -> st + x) 0 t

let store_sample t rgb =
  let brightness = Color.brightness rgb in
  t.(brightness) <- t.(brightness) + 1

let normalize keep t =
  let total = total_samples t in
  if total = 0 then raise (Failure "histgram is empty");
  let cut_samples = truncate ((float total) *. (1.0 -. keep) /. 2.0) in

  let rec find_limit update sum b =
    let sum = sum + t.(b) in
    if sum > cut_samples then b
    else find_limit update sum (update b)
  in

  let min = find_limit ((+) 1) 0 0 
  and max = find_limit ((+) (-1)) 0 255
  in

  if max - min = 0 then fun x -> x
  else
    let new_b_array = Array.init 256 (fun b ->
      if b <= min then 0 
      else if b >= max then 255
      else 255 * (b - min) / (max - min + 1))
    in
    fun rgb ->
      let b = Color.brightness rgb in
      if b = 0 then {r=0; g=0; b=0}
      else begin 
      	let new_b = new_b_array.(b) in
  	let color_fix c =
  	  if c < 0 then 0 else if c > 255 then 255 else c
  	in
  	{ r = color_fix (rgb.r * new_b / b);
  	  g = color_fix (rgb.g * new_b / b);
  	  b = color_fix (rgb.b * new_b / b) }
      end

open OImages

let gamma log img =
  let table =
    Array.init 256 (fun x -> truncate (((float x /. 255.0) ** log) *. 255.0)) in
  let filter =
    fun rgb ->
      { r = table.(rgb.r);
      	g = table.(rgb.g);
      	b = table.(rgb.b) }
  in
  let img' = new rgb24 img#width img#height in
  for x = 0 to img#width - 1 do
    for y = 0 to img#height - 1 do
      img'#unsafe_set x y (filter (img#unsafe_get x y))
    done
  done;
  img'

let filter f keep img =
  let hist = create () in
  for x = 0 to img#width - 1 do
    for y = 0 to img#height - 1 do
      store_sample hist (img#unsafe_get x y)
    done
  done;
(*
  let dx = img#width / 100 in
  let dy = img#height / 100 in
  for x = 0 to (img#width - 1) / dx do
    for y = 0 to (img#height - 1) / dy do
      store_sample hist (img#unsafe_get (x*dx) (y*dy))
    done
  done;
*)
  let filter = f keep hist in
  let img' = new rgb24 img#width img#height in
  for x = 0 to img#width - 1 do
    for y = 0 to img#height - 1 do
      img'#unsafe_set x y (filter (img#unsafe_get x y))
    done
  done;
  img'  
