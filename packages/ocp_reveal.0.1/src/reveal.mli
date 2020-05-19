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

type configuration = {
  (* Display controls in the bottom right corner. *)
  controls : bool;

  (* Display a presentation progress bar. *)
  progress : bool;

  (* Display the page number of the current slide. *)
  slide_number : bool;

  (* Push each slide change to the browser history. *)
  history : bool;

  (* Enable keyboard shortcuts for navigation. *)
  keyboard: bool;

  (* Enable the slide overview mode. *)
  overview: bool;

  (* Vertical centering of slides. *)
  center : bool;

  (* Enables touch navigation on devices with touch input. *)
  touch : bool;

  (* Loop the presentation. *)
  loop : bool;

  (* Change the presentation direction to be RTL. *)
  rtl : bool;

  (* Turns fragments on and off globally. *)
  fragments : bool;

  (* Flags if the presentation is running in an embedded mode,
     i.e. contained within a limited portion of the screen. *)
  embedded : bool;

  (* Flags if we should show a help overlay when the questionmark
     key is pressed *)
  help : bool;

  (* Flags if speaker notes should be visible to all viewers. *)
  show_notes: bool;

  (* Number of milliseconds between automatically proceeding to the
     next slide, disabled when set to 0, this value can be overwritten
     by using a data-autoslide attribute on your slides. *)
  auto_slide: float;

  (* Stop auto-sliding after user input. *)
  auto_slide_stoppable : bool;

  (* Enable slide navigation via mouse wheel. *)
  mouse_wheel : bool;

  (* Hides the address bar on mobile devices. *)
  hide_address_bar : bool;

  (* Opens links in an iframe preview overlay. *)
  preview_links : bool;

  (* Transition style. *)
  transition : Slides.transition;

  (* Transition speed. *)
  transition_speed : Slides.speed;

  (* Transition style for full page slide backgrounds. *)
  background_transition : Slides.transition;

  (* Number of slides away from the current that are visible. *)
  view_distance : int;

  (* Remote control your reveal.js presentation using a touch device. *)
  remote : bool;
}

val default_global_config : configuration

val initialize : ?configuration:configuration -> unit -> unit
