(***********************************************************************)
(*                                                                     *)
(*                             Active-DVI                              *)
(*                                                                     *)
(*                   Projet Cristal, INRIA Rocquencourt                *)
(*                                                                     *)
(*  Copyright 2003 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Lesser General Public License.          *)
(*                                                                     *)
(*  Jun Furuse, Didier Rémy and Pierre Weis.                           *)
(*  Contributions by Roberto Di Cosmo, Didier Le Botlan,               *)
(*  Xavier Leroy, and Alan Schmitt.                                    *)
(*                                                                     *)
(*  Based on Mldvi by Alexandre Miquel.                                *)
(***********************************************************************)

(*  A laser pointer to point to the slides.
    Pierre Weis.                                                       *)

(* $Id$ *)
open Graphics;;

let not_the_pointer_color = Graphics.background;;

let set_pointer_color, get_pointer_color =
  let pointer_color = ref Graphics.red in
  (fun c -> if c > 0 then pointer_color := c),
  (fun () -> !pointer_color);;

let set_pointer_color_string s =
  set_pointer_color (Dvicolor.parse_color s);;

Options.add
  "-laser-pointer-color"
  (Arg.String set_pointer_color_string)
  (Printf.sprintf
     "<color>: set the color of the ``laser'' pointer,\
     \n\t (the default color is red).");;

(* Pointer size is at least 4 *)
let get_pointer_size, set_pointer_size =
  let pointer_size = ref 40 in
  (fun () -> !pointer_size),
  (fun sz -> pointer_size := max 4 sz);;

Options.add
  "-laser-pointer-size"
  (Arg.Int set_pointer_size)
  (Printf.sprintf
     "<size>: set the size of the ``laser'' pointer in points,\
     \n\t (the default size is %d points)." (get_pointer_size ()));;

type pointer = {
  mutable bkg_img : image;
  mutable ptr_img : image;
  mutable x : int;
  mutable y : int;
  half_size : int;
};;

(* Creating the pointer image. *)

(* In the backing store (not to disturb the screen).

   0 - Save some background image somewhere (for instance where is the mouse)
   1 - Clear this area
   2 - Draw the pointer
   3 - Save this image
   4 - Restore the background image *)

let make_white_image h w =
  let pixmap = Array.make_matrix h w not_the_pointer_color in
  make_image pixmap;;

let color_as_transp c img =
  let pixmap = dump_image img in
  for i = 0 to Array.length pixmap - 1 do
    let l = pixmap.(i) in
    for j = 0 to Array.length l - 1 do
      if l.(j) = c then l.(j) <- transp
    done
  done;
  make_image pixmap;;

(* Draw the pointer with lower left corner at (x, y). *)
let draw_pointer x y w =
  let r = w / 2 - 2 in
  let d = r / 2 in
  set_color not_the_pointer_color;
  fill_rect x y w w;
  set_color (get_pointer_color ());
  let xc = x + r + 1
  and yc = y + r + 1 in
  fill_circle xc yc r;
  set_color not_the_pointer_color;
  fill_circle xc yc d;;

(* Create a pointer with lower left corner at (x, y). *)
let create_pointer x y w =
  (* Save the initial background. *)
  let bkg_img = get_image x y w w in
  (* Draw the pointer, centered at (x, y). *)
  draw_pointer x y w;
  (* Get the corresponding image. *)
  let pointer_image =
    color_as_transp not_the_pointer_color (get_image x y w w) in
  (* Restore the background. *)
  draw_image bkg_img x y;
  { bkg_img = bkg_img;
    ptr_img = pointer_image;
    x = x; y = y;
    half_size = w / 2;
  };;

(* Clear the actual pointer. *)
let clear_pointer ptr = draw_image ptr.bkg_img ptr.x ptr.y;;

(* The X11 pointer shifts with respect to Advi's laser pointer center. *)
let xptr_x = 1
and xptr_y = 1;;

(* Show the pointer: first save the background, then draw the pointer. *)
let show_pointer ptr x y =
  clear_pointer ptr;
  let r = ptr.half_size in
  (* Compute the actual lower left corner of the pointer. *)
  let x = x - r + xptr_x
  and y = y - r + xptr_y in
  (* Save the actual background. *)
  blit_image ptr.bkg_img x y;
  (* Move the pointer. *)
  ptr.x <- x; ptr.y <- y;
  draw_image ptr.ptr_img x y;;

(* An ``impossible'' key struck value. *)
let null_key = '\000';;

let rec treat_laser_event ptr q =
   match wait_next_event
           [Mouse_motion; Button_down; Button_up; Key_pressed;] with
   | { mouse_x = x; mouse_y = y;
       button = btn;
       keypressed = kp;
       key = c; } ->
       show_pointer ptr x y;
       if kp then begin
         match c with
         | '' | '' -> raise Exit
         | '' when q = '' ->
            Shot.save_page_image ();
            clear_pointer ptr;
            GraphicsY11.anti_synchronize ();
            show_pointer ptr x y
         | '' when q = '' ->
            Misc.push_key_event '' GraphicsY11.control;
            Misc.push_key_event '' GraphicsY11.control;
            raise Exit
         | '' -> treat_laser_event ptr c
         | 'l' when q = '' -> raise Exit
         | c ->
            go_on ();
            Misc.push_key_event c (GraphicsY11.get_modifiers ());
            if q = '' then Misc.push_key_event '' GraphicsY11.control;
            raise Exit
       end else begin
         go_on ();
         Misc.push_mouse_event x y btn;
         raise Exit
       end

and go_on () =
  Misc.push_key_event 'l' GraphicsY11.nomod;
  Misc.push_key_event '' GraphicsY11.control;;

let switch_on_laser_beam () =

 let x0 = Graphics.size_x () / 2
 and y0 = Graphics.size_y () / 2 in
 let laser_pointer =
   GraphicsY11.only_on_backing_store
     (create_pointer x0 y0) (get_pointer_size ()) in

 let x, y = mouse_pos () in
 show_pointer laser_pointer x y;

 try while true do treat_laser_event laser_pointer null_key done with
 | Exit -> clear_pointer laser_pointer;;

let laser_beam =
  Busy.with_cursor Busy.Pointer
   (GraphicsY11.only_on_screen switch_on_laser_beam);;
