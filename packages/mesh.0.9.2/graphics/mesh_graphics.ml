(* File: mesh_graphics.ml

   Copyright (C) 2008-

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: http://math.umons.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License version 2.1 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)


(** Displaying the mesh with Graphics *)

open Printf
open Bigarray
open Graphics

(* TODO:

   - allow to zoom in and out
   - display the segments in another color
   - allow to switch on and off the nodes #
*)

let draw (type l) ?width ?height ?color ?points ?point_idx ?triangle_idx
      ?voronoi ?point_marker_color
      (mesh: l Mesh.t) =
  match Mesh.layout mesh with
  | C_layout ->
     Mesh_graphicsC.draw ?width ?height ?color ?points ?point_idx ?triangle_idx
       ?voronoi ?point_marker_color mesh
  | Fortran_layout ->
     Mesh_graphicsF.draw ?width ?height ?color ?points ?point_idx ?triangle_idx
       ?voronoi ?point_marker_color mesh

let init_graph width height =
  let xbd = 10 and ybd = 10 in
  (* Drawing itself *)
  open_graph (sprintf " %ix%i-40+40" (width + 2 * xbd) (height + 2 * ybd));
  moveto xbd ybd

let hold_graph () =
  (* Wait for the key 'q' to be pressed. *)
  try
    while true do
      let status = wait_next_event [Button_down; Key_pressed] in
      if status.button
        || (status.keypressed && (status.key = 'q' || status.key = 'Q')) then (
        close_graph();
        raise Exit
      );
    done
  with Exit -> ()

let display ?(width=600) ?(height=600) ?color ?points ?point_idx ?triangle_idx
            ?voronoi ?point_marker_color mesh =
  init_graph width height;
  set_window_title("Mesh (" ^ Filename.basename Sys.argv.(0) ^ ")");
  draw ~width ~height ?color ?points ?point_idx ?triangle_idx
       ?voronoi ?point_marker_color
       mesh;
  hold_graph()

let level_curves (type l) ?(width=600) ?(height=600) ?boundary (mesh: l Mesh.t)
                 (z: l Mesh.vec) ?level_eq levels =
  match Mesh.layout mesh with
  | C_layout ->
     Mesh_graphicsC.level_curves ~width ~height ?boundary
       mesh z ?level_eq levels
  | Fortran_layout ->
     Mesh_graphicsF.level_curves ~width ~height ?boundary
       mesh z ?level_eq levels

let display_level_curves ?(width=600) ?(height=600) ?boundary mesh z
    ?level_eq levels =
  init_graph width height;
  set_window_title("Mesh (" ^ Filename.basename Sys.argv.(0) ^ ")");
  level_curves ~width ~height ?boundary mesh z ?level_eq levels;
  hold_graph()

let super_level (type l) ?(width=600) ?(height=600) ?boundary (mesh: l Mesh.t)
                (z: l Mesh.vec) level color =
  match Mesh.layout mesh with
  | C_layout ->
     Mesh_graphicsC.super_level ~width ~height ?boundary mesh z level color
  | Fortran_layout ->
     Mesh_graphicsF.super_level ~width ~height ?boundary mesh z level color

let sub_level (type l) ?(width=600) ?(height=600) ?boundary (mesh: l Mesh.t)
              (z: l Mesh.vec) level color =
  match Mesh.layout mesh with
  | C_layout -> Mesh_graphicsC.sub_level ~width ~height ?boundary
                  mesh z level color
  | Fortran_layout -> Mesh_graphicsF.sub_level ~width ~height ?boundary
                        mesh z level color
