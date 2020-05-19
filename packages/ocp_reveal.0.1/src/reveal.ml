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

open Js_of_ocaml

let doc = Dom_html.document
let _s = Js.string

type configuration = {
  controls              : bool;
  progress              : bool;
  slide_number          : bool;
  history               : bool;
  keyboard              : bool;
  overview              : bool;
  center                : bool;
  touch                 : bool;
  loop                  : bool;
  rtl                   : bool;
  fragments             : bool;
  embedded              : bool;
  help                  : bool;
  show_notes            : bool;
  auto_slide            : float;
  auto_slide_stoppable  : bool;
  mouse_wheel           : bool;
  hide_address_bar      : bool;
  preview_links         : bool;
  transition            : Slides.transition;
  transition_speed      : Slides.speed;
  background_transition : Slides.transition;
  view_distance         : int;
  remote                : bool;
}

class type math = object
  method config : Js.js_string Js.t Js.prop
end

class type dependencies = object
  method src : Js.js_string Js.t Js.prop
  method condition : unit -> bool Js.t Js.prop
  method callback : unit -> bool Js.t Js.prop
  method async : bool Js.t Js.prop
end

class type reveal = object
  method controls: bool Js.t Js.prop
  method progress: bool Js.t Js.prop
  method slideNumber: bool Js.t Js.prop
  method history: bool Js.t Js.prop
  method keyboard: bool Js.t Js.prop
  method overview: bool Js.t Js.prop
  method center: bool Js.t Js.prop
  method touch: bool Js.t Js.prop
  method loop: bool Js.t Js.prop
  method rtl: bool Js.t Js.prop
  method fragments: bool Js.t Js.prop
  method embedded: bool Js.t Js.prop
  method help: bool Js.t Js.prop
  method showNotes: bool Js.t Js.prop
  method autoSlide: float Js.prop
  method autoSlideStoppable : bool Js.t Js.prop
  method mouseWheel: bool Js.t Js.prop
  method hideAddressBar: bool Js.t Js.prop
  method previewLinks: bool Js.t Js.prop
  method transition: Js.js_string Js.t Js.prop
  method transitionSpeed: Js.js_string Js.t Js.prop
  method backgroundTransition: Js.js_string Js.t Js.prop
  method viewDistance: int Js.prop
  method remote: bool Js.t Js.prop
  method math: math Js.t Js.prop
  method dependencies: dependencies Js.t Js.js_array Js.t Js.prop
end

let default_global_config = {
  controls = true;
  progress = true;
  slide_number = true;
  history = false;
  keyboard = true;
  overview = true;
  center = true;
  touch = true;
  loop = false;
  rtl = false;
  fragments = true;
  embedded = false;
  help = true;
  show_notes= false;
  auto_slide= 0.;
  auto_slide_stoppable = true;
  mouse_wheel = false;
  hide_address_bar = true;
  preview_links = false;
  transition = Slides.None;
  transition_speed = Slides.Default;
  background_transition = Slides.None;
  view_distance = 3;
  remote = true;
}

let dep ?(async=None) ?(condition=None) ?(callback=None) src =
  let obj = Js.Unsafe.obj [||] in
  obj##.src := _s src;
  begin match async with
  | None -> ()
  | Some async -> obj##.async := async
  end;
  begin match condition with
  | None -> ()
  | Some condition -> obj##.condition := condition
  end;
  begin match callback with
  | None -> ()
  | Some callback -> obj##.callback := callback
  end;
  obj


let initialize ?configuration:(c=default_global_config) () =
  let open External_js in

  (* Interpret latex math with MathJax *)
  let math : math Js.t = Js.Unsafe.obj [||] in
  math##.config := _s "TeX-AMS_HTML-full";

  let deps = Js.array [|

    (** Cross-browser shim that fully implements classList -
        https://github.com/eligrey/classList.js/ *)
    dep
      ~async:None
      ~condition:(Some (fun () ->
        doc##.body##.classList##.length <> 0))
      classList_js;

    (** Interpret Markdown in <section> elements. *)
    dep
      ~condition:(Some (fun () -> doc##querySelector (_s "[data-markdown]")))
      marked_js;
    dep
      ~async:None
      ~condition:(Some (fun () -> doc##querySelector (_s "[data-markdown]")))
      markdown_js;

    (** Syntax highlight for <code> elements. *)
    dep
      ~callback:(Some (fun () -> (Js.Unsafe.global##.hljs)##initHighlighting()))
      highlight_js;

    (** Speaker notes. *)
    dep note_js;

    (** MathJax : math equations (cf latex syntax) *)
    dep math_js;

    (** Zoom in and out with Alt+click *)
    dep ~async:(Some Js._true) zoom_js |] in

  let reveal : reveal Js.t = Js.Unsafe.obj [||] in
  reveal##.controls := Js.bool c.controls;
  reveal##.progress := Js.bool c.progress;
  reveal##.slideNumber := Js.bool c.slide_number;
  reveal##.history := Js.bool c.history;
  reveal##.keyboard := Js.bool c.keyboard;
  reveal##.overview := Js.bool c.overview;
  reveal##.center := Js.bool c.center;
  reveal##.touch := Js.bool c.touch;
  reveal##.loop := Js.bool c.loop;
  reveal##.rtl := Js.bool c.rtl;
  reveal##.fragments := Js.bool c.fragments;
  reveal##.embedded := Js.bool c.embedded;
  reveal##.help := Js.bool c.help;
  reveal##.showNotes := Js.bool c.show_notes;
  reveal##.autoSlide := c.auto_slide;
  reveal##.autoSlideStoppable := Js.bool c.auto_slide_stoppable;
  reveal##.mouseWheel := Js.bool c.mouse_wheel;
  reveal##.hideAddressBar := Js.bool c.hide_address_bar;
  reveal##.previewLinks := Js.bool c.preview_links;
  reveal##.transition := _s @@ Slides.string_of_transition c.transition;
  reveal##.transitionSpeed := _s @@ Slides.string_of_speed c.transition_speed;
  reveal##.backgroundTransition :=
    _s @@ Slides.string_of_transition c.background_transition;
  reveal##.viewDistance := c.view_distance;
  reveal##.remote := Js.bool c.remote;
  reveal##.math := math;
  reveal##.dependencies := deps;

  (* We have to wait until all initialization scripts are correctly loaded. *)
  Dom_html.window##.onload :=
    Dom_html.handler (fun _ ->
      ignore (Js.Unsafe.global##.Reveal##initialize reveal);
      Js.bool true);
