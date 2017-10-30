(* File: mesh_display.ml

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
open Graphics
open Mesh_utils
open Mesh_common

(* TODO:

   - allow to zoom in and out
   - display the segments in another color
   - allow to switch on and off the nodes #
*)

let draw ?width ?height ?color ?points ?point_idx ?triangle_idx ?voronoi
         ?point_marker_color
         (mesh: 'a #Mesh.t) =
  if Mesh.is_c_layout mesh then
    Mesh_displayC.draw ?width ?height ?color ?points ?point_idx ?triangle_idx
                       ?voronoi ?point_marker_color
                       (mesh_to_c mesh)
  else
    Mesh_displayF.draw ?width ?height ?color ?points ?point_idx ?triangle_idx
                       ?voronoi ?point_marker_color
                       (mesh_to_fortran mesh)

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

let level_curves ?(width=600) ?(height=600) ?boundary (mesh: 'a #Mesh.t)
                 (z: 'a Mesh.vec) ?level_eq levels =
  if Mesh.is_c_layout mesh then
    Mesh_displayC.level_curves ~width ~height ?boundary
                               (mesh_to_c mesh) (vec_to_c z) ?level_eq levels
  else
    Mesh_displayF.level_curves ~width ~height ?boundary
                               (mesh_to_fortran mesh) (vec_to_fortran z)
                               ?level_eq levels

let display_level_curves ?(width=600) ?(height=600) ?boundary mesh z
    ?level_eq levels =
  init_graph width height;
  set_window_title("Mesh (" ^ Filename.basename Sys.argv.(0) ^ ")");
  level_curves ~width ~height ?boundary mesh z ?level_eq levels;
  hold_graph()

let super_level ?(width=600) ?(height=600) ?boundary (mesh: 'a #Mesh.t)
                (z: 'a Mesh.vec) level color =
  if Mesh.is_c_layout mesh then
    Mesh_displayC.super_level ~width ~height ?boundary
                              (mesh_to_c mesh) (vec_to_c z) level color
  else
    Mesh_displayF.super_level ~width ~height ?boundary
                              (mesh_to_fortran mesh) (vec_to_fortran z)
                              level color

let sub_level ?(width=600) ?(height=600) ?boundary (mesh: 'a #Mesh.t)
              (z: 'a Mesh.vec) level color =
  if Mesh.is_c_layout mesh then
    Mesh_displayC.sub_level ~width ~height ?boundary
                            (mesh_to_c mesh) (vec_to_c z) level color
  else
    Mesh_displayF.sub_level ~width ~height ?boundary
                            (mesh_to_fortran mesh) (vec_to_fortran z)
                            level color
