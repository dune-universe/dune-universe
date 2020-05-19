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

open Omd

type transition = None | Fade | Slide | Convex | Concave | Zoom

type speed = Default | Fast | Slow

type color = Black | White | Blue | Red | Green | Yellow | Color of string

type theme =
  Black_theme
| White_theme
| Night_theme
| Blood_theme
| Custom of string

type path = string (* xxx maybe use a special type path ?  *)

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

let title1 t = H1 [Text t]
let title2 t = H2 [Text t]
let title3 t = H3 [Text t]
let title4 t = H4 [Text t]
let title5 t = H5 [Text t]
let title6 t = H6 [Text t]

let text text = Text text
let bold text = Bold [Text text]
let emph text = Emph [Text text]

let paragraph pars = Paragraph pars

let itemize items = Omd.Ul (List.map (fun itemize -> [text itemize]) items)
let enumerate items = Omd.Ol (List.map (fun itemize -> [text itemize]) items)

let string_of_transition = function
  | None    -> "none"
  | Fade    -> "fade"
  | Slide   -> "slide"
  | Convex  -> "convex"
  | Concave -> "concave"
  | Zoom    -> "zoom"

let string_of_speed = function
  | Default -> "default"
  | Fast    -> "fast"
  | Slow    -> "slow"

let string_of_color = function
  | Black   -> "Black"
  | White   -> "White"
  | Blue    -> "Blue"
  | Red     -> "Red"
  | Green   -> "Green"
  | Yellow  -> "Yellow"
  | Color s -> s

let default = {
  title = Text "";
  content = "";
  transition = Slide;
  video = None;
  background_color = None;
  text_color = Black;
  background_img = None;
  background_video = None;
  background_embed = None;
  theme = Custom "./ocamlpro.css";
}

let slide = default
let convex  = { default with transition = Convex }
let concave = { default with transition = Concave }
let fade    = { default with transition = Fade }
let zoom    = { default with transition = Zoom }

let pause str =
  Printf.sprintf "%s%s\n\n\n"
    str
    " <!-- .element: class=\"fragment\" --> "

let slides_ref : slide_t list ref = ref []

let frame sec =
  let content = of_string sec.content in
  let slide = Single ([
    sec.title;
    paragraph content
  ], sec) in
  slides_ref := slide :: !slides_ref;
  slide

let from_file file =
 let load_file f =
   let ic = open_in f in
   let n = in_channel_length ic in
   let s = Bytes.create n in
   really_input ic s 0 n;
   close_in ic;
   Bytes.to_string s in
 Omd.of_string (load_file file)
