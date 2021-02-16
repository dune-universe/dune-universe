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

val draw_tex : Cairo.context -> Gentex.t -> unit

module MetaPath : sig
  type pen = Matrix.t

  val stroke : Cairo.context -> pen -> Spline_lib.path -> unit

  val fill : Cairo.context -> Spline_lib.path -> unit

  val draw_path : Cairo.context -> Spline_lib.path -> unit
end

module Picture : sig
  val draw : Cairo.context -> float -> float -> Picture_lib.t -> unit

  val where :
    Cairo.context -> Picture_lib.t -> float * float -> Picture_lib.id list

  val move :
    Cairo.context ->
    Picture_lib.t ->
    Picture_lib.id ->
    float * float ->
    float * float
end
