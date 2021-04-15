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

open Transitions;;

(* Drawing (moving and resizing and ...) sprites on the screen. *)
let prev_geom = ref None;;

let init_sprite () = prev_geom := None;;

let do_on_screen = GraphicsY11.only_on_screen;;

let do_on_screen f x =
  Graphics.remember_mode false;
  GraphicsY11.display_mode true;
  let r = f x in
  GraphicsY11.display_mode false;
  Graphics.remember_mode true;
  r;;

let draw_sprite newimg x y width height =
  let orgimg = Graphics.get_image x y width height in
  let (wx, wy, wwidth, wheight) =
    match !prev_geom with
    | None -> x, y, width, height 
    | Some (x', y', width', height') ->
	let x'' = min x x'
	and y'' = min y y' in
	let width''  = max (x + width - 1) (x' + width' - 1) - x'' + 1
	and height'' = max (y + height - 1) (y' + height' - 1) - y'' + 1 in
	x'', y'', width'', height''
  in
  Graphics.draw_image newimg x y;
  let workimg = Graphics.get_image wx wy wwidth wheight in
  Graphics.draw_image orgimg x y;
  do_on_screen (Graphics.draw_image workimg wx) wy;
  prev_geom := Some (x, y, width, height)
;;

(* Moving slides or part of slides on the screen,
   as specified by the transition method. *)
let sleep = ref (fun _ -> false);;

let current_transition = ref TransNone;;
 
let slide delay steps from =
  (* against full screen *)
  let w = Graphics.size_x () and h = Graphics.size_y () in
  let img = Graphics.get_image 0 0 w h in
  try
    for i = steps - 1 downto 1 do
      if delay <> 0.0 then if !sleep delay then raise Exit;
      let px, py =
  	match from with
  	| DirLeft -> -w / steps * i, 0
  	| DirBottom -> 0, -h / steps * i
  	| DirTop -> 0, h / steps * i
  	| DirTopLeft -> -w / steps * i, h / steps * i
  	| DirTopRight -> w / steps * i, h / steps * i
  	| DirBottomRight -> w / steps * i, -h / steps * i
  	| DirBottomLeft -> -w / steps * i, -h / steps * i
  	| DirRight | _ -> w / steps * i, 0
      in
      Graphics.draw_image img px py;
    done
  with
  | Exit -> ()
;;

let wipe delay steps from (w, h) (x, y) =
  (* bit naive implementation (=inefficient) *)
  try
    for i = steps - 1 downto 1 do
      if !sleep delay then raise Exit;
      let px, py, w, h =
  	match from with
  	| DirLeft -> -w / steps * i, 0, w, h
  	| DirBottom -> 0, -h / steps * i, w, h
  	| DirTop -> 0, h / steps * i, w, h
  	| DirTopLeft -> -w / steps * i, h / steps * i, w, h
  	| DirTopRight -> w / steps * i, h / steps * i, w, h
  	| DirBottomRight -> w / steps * i, -h / steps * i, w, h
  	| DirBottomLeft -> -w / steps * i, -h / steps * i, w, h
  	| DirCenter -> 
  	    let j = steps - i in
  	    w / 2 - w / (steps * 2) * j,
            h / 2 - h / (steps * 2) * j,
  	    w / steps * j, h / steps * j
  	| DirRight | _ -> w / steps * i, 0, w, h
      in
      if w = 0 || h = 0 then () else
      let img = Graphics.get_image (px + x) (py + y) w h in
      Graphics.draw_image img (px + x) (py + y);
    done
  with
  | Exit -> ()
;;

let block delay steps from (w, h) (x, y) =
  let rec find_division dx dy =
    if dx * dy > steps then dx, dy else
    (* try to add dx 1 *)
    let dx1 = dx + 1 in
    let w_dx1 = w / dx1 in
    let dy1 = if w_dx1 = 0 then 100000 else h / (w / dx1) + 1 in
    (* try to add dy 1 *)
    let dy2 = dy + 1 in
    let h_dy2 = h / dy2 in
    let dx2 = if h_dy2 = 0 then 100000 else w / (h / dy2) + 1 in
    if dx1 * dy1 < dx2 * dy2 then find_division dx1 dy1 
    else find_division dx2 dy2
  in
  let dx, dy =
    let dx, dy = find_division 1 1  in
    let dx = if dx = 0 then 1 else dx
    and dy = if dy = 0 then 1 else dy in 
    if dx * dy > w * h then w, h else dx, dy
  in

  let bw = w / dx + (if w mod dx = 0 then 0 else 1) in
  let bh = h / dy + (if h mod dy = 0 then 0 else 1) in

  let img = Graphics.create_image bw bh in

  let swap order a b =
    let tmp = order.(a) in
    order.(a) <- order.(b);
    order.(b) <- tmp
  in

  let order =
    let priority =
      match from with
      | DirLeft -> fun x y -> x
      |	DirRight -> fun x y -> -x
      | DirBottom -> fun x y -> y
      |	DirTop -> fun x y -> -y
      |	DirTopLeft -> fun x y -> x - y
      |	DirTopRight -> fun x y -> -x - y
      |	DirBottomLeft -> fun x y -> x + y
      |	DirBottomRight -> fun x y -> -x + y
      |	DirCenter -> fun x y -> abs (x - dx / 2) + abs (y - dy / 2)
      | DirNone -> fun x y -> Random.int (dx * dy)
    in
    let order = Array.init (dx * dy) (fun i -> 
      let x = i mod dx and y = i / dx in 
      priority x y, (x, y) )
    in
    Array.sort compare order;
    for i = 0 to dx * dy - 1 do
      let j = i + Random.int (dx * dy / 20) in
      if j < 0 || j >= dx * dy then ()
      else swap order i j
    done;
    order
  in
  try
    for i = 0 to dx * dy - 1 do
      if !sleep delay then raise Exit;
      let _, (bx, by) = order.(i) in
      Graphics.blit_image img (bx * bw + x) (by * bh + y);
      Graphics.draw_image img (bx * bw + x) (by * bh + y);
    done
  with Exit -> ()
;;

(* Moving images along a general user's specified path. *)

(* Collection of predefined path generators wired in. *)
let get_genpath = function 
  | "spiral" ->
     (fun steps (sx, sy, ss, sr) (tx, ty, ts, tr) i ->
        failwith "Spiral not yet implemented") 
  | "line" ->
     (fun steps (sx, sy, ss, sr) (tx, ty, ts, tr) ->
        let stepx = (tx -. sx) /. float steps
        and stepy = (ty -. sy) /. float steps in
        let steps = (ts -. ss) /. float steps
        and stepr = (tr -. sr) /. float steps in
        fun i ->
          let x, y = 
            sx +. float i *. stepx,
            sy +. float i *. stepy in
          let s, r =
            ss +. float i *. steps,
            sr +. float i *. stepr in
          (x, y, s, r))
  | s -> failwith ("Unknown path " ^ s)
;;

(* Scaling sprites. *)
let rescale_grimage img w h nw nh =
  if nw = w && nh = h then img else
  let cimg = Graphic_image.image_of img in
  Graphic_image.of_image (Images.Rgb24 (Rgb24.resize None cimg nw nh))
;;

(* Rendering function for sprites along a path *)
let render newimg w h
  (nextx, nexty, nextscale, nextrot) =
  let nw = Misc.round (float w *. nextscale)
  and nh = Misc.round (float h *. nextscale) in
  let newimg = rescale_grimage newimg w h nw nh in
  draw_sprite newimg
    (Misc.round nextx) (Misc.round nexty)
    nw nh
;;

(* Moving image img along path generated by genpath. *)
let move_along_path newimg w h delay steps genpath start stop =
  let genpath = get_genpath genpath steps start stop in
  let rec loop prev i =
    if i < steps then begin
      if !sleep delay then raise Exit;
      let next = genpath i in
      render newimg w h next;
      loop next (i + 1) end in
  try loop start 0 with
  | Exit -> ()
;;

let path delay steps genpath start stop =
  (* against full screen *)
  let w = Graphics.size_x () and h = Graphics.size_y () in
  let newimg = Graphics.get_image 0 0 w h in
  move_along_path newimg w h delay steps genpath start stop;;

let get_steps default = function Some x -> x | None -> default;;

let pathelem_inst (optx, opty, opts, optr) (x, y, s, r) =
  let l = 
    List.map
      (function
       | Some v, _ -> v
       | None, v -> v)
      (List.combine [optx; opty; opts; optr] [x; y; s; r]) in
  match l with
  | [actx; acty; acts; actr] -> (actx, acty, acts, actr)
  | _ -> assert false
;;

let synchronize_transition () =
  if !current_transition <> TransNone then
  let w = Graphics.size_x () and h = Graphics.size_y () in
  do_on_screen (fun () ->
    match !current_transition with
    | TransSlide (steps, from) ->
        slide 0.0 (get_steps 20 steps) from
    | TransWipe (steps, from) ->
        wipe 0.0 (get_steps 20 steps) from (w, h) (0, 0)
    | TransBlock (steps, from) ->
        block 0.0 (get_steps 5000 steps) from (w, h) (0, 0)
    | TransPath (steps, genpath, start, stop) ->
        path 0.0 (get_steps 20 steps) genpath
	  (pathelem_inst start (float w, float h, 1.0, 0.0))
	  (pathelem_inst stop  (0.0, 0.0, 1.0, 0.0))
    | TransNone -> assert false
  ) ()
;;

let string_of_transmode = function
  | TransNone -> "none"
  | TransSlide _ -> "slide"
  | TransBlock _ -> "block"
  | TransWipe _ -> "wipe"
  | TransPath _ -> "path"
;;

(* Argument oldimg will be useful to transitions with alpha blending,
   if we add them in the future. *)
let box_transition trans oldimg newimg x y width height =
  let screen_w = Graphics.size_x () and screen_h = Graphics.size_y () in
  init_sprite ();
  match trans with
  | TransNone -> ()
  | TransSlide (steps, from) ->
      let steps = get_steps 20 steps in
      let calc_new_steps_and_speed len =
        let steps = if steps <= 0 then 1 else steps in
        let speed = max (len / steps) 1 in
        let newsteps = len / speed + 1 in
        newsteps, speed
      in
      let f, newsteps = 
	match from with
	| DirRight -> 
	    let len = screen_w - x + width - 1 in
            let newsteps, speed = calc_new_steps_and_speed len in
	    (fun i ->
               draw_sprite newimg (x + i * speed) y width height), newsteps
	| DirLeft ->
	    let len = x in
            let newsteps, speed = calc_new_steps_and_speed len in
	    (fun i ->
               draw_sprite newimg (x - i * speed) y width height), newsteps
	| DirTop -> 
	    let len = screen_h - y + height - 1 in
            let newsteps, speed = calc_new_steps_and_speed len in
	    (fun i ->
               draw_sprite newimg x (y + i * speed) width height), newsteps
	| DirBottom | _ ->
	    let len = y in
            let newsteps, speed = calc_new_steps_and_speed len in
	    (fun i ->
               draw_sprite newimg x (y - i * speed) width height), newsteps
      in
      begin try 
	for i = newsteps - 1 downto 1 do 
	  if !sleep 0.01 then raise Exit
	  else f i
	done
      with
      |	Exit -> ()
      end
  | TransPath (steps, genpath, start, stop) ->
      let steps = get_steps 50 steps in
      do_on_screen (fun stop ->
        move_along_path newimg width height 0.01 steps genpath 
          (pathelem_inst start (float x, float y, 1.0, 0.0))
          (pathelem_inst stop  (float x, float y, 1.0, 0.0))
	  ) stop
  | TransBlock (step, from) ->
      let step = get_steps 50 step in
      do_on_screen (fun () ->
        block 0.01 step from (width, height) (x, y)) ()
  | TransWipe (step, from) ->
      let step = get_steps 50 step in
      do_on_screen (fun () ->
        wipe 0.01 step from (width, height) (x, y)) ()
;;

let saved_transbox = ref None;;

let transbox_save x y width height =
  let x = x and y = y - 1 and width = width + 1 and height = height + 2 in
  let img = Graphics.get_image x y width height in
  saved_transbox := Some (img, x, y, width, height)
;;

let transbox_go trans = 
  begin match !saved_transbox with
  | Some (oldimg, x, y, width, height) ->
      let newimg = Graphics.get_image x y width height in
      box_transition trans oldimg newimg x y width height 
  | None -> assert false
    (* ??? forgot to call transbox_save before ??? *)
    (* ??? Seems to be room for simplification there: transbox_save should not
       be called from within tex but as the first action of trans_go ??? *)
  end;
  saved_transbox := None;
;;
