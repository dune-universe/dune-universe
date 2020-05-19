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

let (/) = Filename.concat

let root = "./"

let js = root / "js"
let css = root / "css"
let theme = root / "theme"

(* CSS files *)
let reveal_css         = css / "reveal.css"
let reveal_zenburn_css = css / "zenburn.css"

(* JS files *)
let reveal_js      = js / "reveal.js"
let reveal_head    = js / "head.min.js"
let classList_js   = js / "classList.js"
let marked_js      = js / "marked.js"
let markdown_js    = js / "markdown.js"
let highlight_js   = js / "highlight.js"
let note_js        = js / "notes.js"
let note_client_js = js / "client.js"
let math_js        = js / "math.js"
let zoom_js        = js / "zoom.js"
let remote_js      = js / "remotes.js"

(* Theme *)
let reveal_theme   = theme / "reveal.min.css"
let ocamlpro_theme = theme / "ocamlpro.css"
let white_theme    = theme / "white.css"
let black_theme    = theme / "black.css"
let night_theme    = theme / "night.css"
let blood_theme    = theme / "blood.css"
