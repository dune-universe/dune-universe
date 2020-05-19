(*****************************************************************************)
(*                                                                           *)
(*  Copyright 2015 OCamlPro                                                  *)
(*                                                                           *)
(*  All rights reserved.  This file is distributed under the terms of        *)
(*  the Lesser GNU Public License version 3.0.                               *)
(*                                                                           *)
(*  This software is distributed in the hope that it will be useful,         *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of           *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *)
(*  Lesser GNU General Public License for more details.                      *)
(*                                                                           *)
(*****************************************************************************)

type transition = None | Fade | Slide | Convex | Concave | Zoom

type speed = Default | Fast | Slow

type color = Black | White | Blue | Red | Green | Yellow | Color of string

type theme =
  Black_theme
| White_theme
| Night_theme
| Blood_theme
| Custom of string

type path = string

type slide = {
  title : Omd.element;
  content : string;
  transition : transition;
  video: path option;
  text_color : color;                   (* xxx TODO *)
  background_color : color option;
  background_img : path option;
  background_video : path option;
  background_embed : path option;
  theme : theme;
}

type slide_t =
| Single of (Omd.t * slide)  (* One single slide *)
| Multiple of slides_t       (* Vertical navigation for subsections *)
| File of (Omd.t * slide)    (* xxx TODO Load content from file *)

and slides_t = slide_t list

val title1 : string -> Omd.element
val title2 : string -> Omd.element
val title3 : string -> Omd.element
val title4 : string -> Omd.element
val title5 : string -> Omd.element
val title6 : string -> Omd.element

val text : string -> Omd.element
val bold : string -> Omd.element
val emph : string -> Omd.element

val paragraph : Omd.t -> Omd.element
val itemize : string list -> Omd.element
val enumerate : string list -> Omd.element

val string_of_transition : transition -> string
val string_of_speed : speed -> string
val string_of_color : color -> string

(** Predefine slide named by effect transition. *)
val default : slide
val slide : slide
val convex : slide
val concave : slide
val fade : slide
val zoom : slide

val pause : string -> string
val slides_ref : slide_t list ref

(** Create a new frame. *)
val frame : slide -> slide_t

val from_file : path -> Omd.t
