(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Johannes Kanig, Stephane Lescuyer                       *)
(*  Jean-Christophe Filliatre, Romain Bardou and Francois Bobot           *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

open Types
module Pen = Pen

module Dash = struct
  include Dash

  let scaled = mkDScaled
end

type t = brush

let opt_def def = function None -> def | Some s -> s

let opt_map f = function None -> None | Some s -> Some (f s)

let t color ?pen ?dash ?scale ?brush () =
  match scale with
  | None -> mkBrushOpt brush color pen dash
  | Some s ->
      mkBrushOpt brush color
        (Some (Pen.scale s (opt_def Pen.default pen)))
        (opt_map (Dash.scaled s) dash)

(** {2 Predefined Colors} *)
type brush_colored =
  ?pen:Pen.t -> ?dash:Dash.t -> ?scale:Num.t -> ?brush:t -> unit -> t
(** {3 base colors} *)

let white = t (Some Color.white)

let black = t (Some Color.black)

let red = t (Some Color.red)

let blue = t (Some Color.blue)

let green = t (Some Color.green)

let cyan = t (Some Color.cyan)

let yellow = t (Some Color.yellow)

let magenta = t (Some Color.magenta)

(** {3 lighter colors} *)

let lightred = t (Some Color.lightred)

let lightblue = t (Some Color.lightblue)

let lightgreen = t (Some Color.lightgreen)

let lightcyan = t (Some Color.lightcyan)

let lightyellow = t (Some Color.lightyellow)

let lightmagenta = t (Some Color.lightmagenta)

(** {3 grays} *)

let gray f = t (Some (Color.gray f))

let lightgray = t (Some Color.lightgray)

let mediumgray = t (Some Color.mediumgray)

let darkgray = t (Some Color.darkgray)

(** {3 additional colors} *)

let orange = t (Some Color.orange)

let purple = t (Some Color.purple)

let t ?color = t color

let color t = t.Hashcons.node.color

let pen (t : t) : Pen.t option = t.Hashcons.node.pen

let dash t = t.Hashcons.node.dash
