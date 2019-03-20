(* File: mesh_easymesh.ml

   Copyright (C) 2006-

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

(** Put the full path if not in your search path. *)
let easymesh = "EasyMesh.exe"

open Printf
open Bigarray

class ['l] pslg = ['l] Mesh.pslg

let pslg = Mesh.pslg
let create = Mesh.create

(* Interface to easymesh -- gathering the FORTRAN and C layouts together
 ***********************************************************************)

let read (type l) (layout: l layout) fname : l Mesh.t =
  match layout with
  | C_layout -> Mesh_easymeshC.read Mesh_easymeshC.empty_pslg fname
  | Fortran_layout -> Mesh_easymeshF.read Mesh_easymeshF.empty_pslg fname

let triangulate (type l) ~max_area (pslg: l pslg) =
  (* Save domain file *)
  let (fname_plsg, fh) = Filename.open_temp_file "EasyMesh" ".d" in
  let fname = Filename.chop_extension fname_plsg in
  (match Mesh.layout pslg with
   | C_layout -> Mesh_easymeshC.output_pslg fh pslg max_area
   | Fortran_layout -> Mesh_easymeshF.output_pslg fh pslg max_area);
  close_out fh;
  (* Execute easymesh *)
  let _ = Sys.command (sprintf "%s %s -m" easymesh fname) in
  (* The return code of EasyMesh is unrelialble, do not check it. *)
  (* Read the result *)
  let mesh : l Mesh.t = match Mesh.layout pslg with
    | C_layout -> Mesh_easymeshC.read pslg fname
    | Fortran_layout -> Mesh_easymeshF.read pslg fname in
  Sys.remove (fname_plsg);
  Sys.remove (fname ^ ".n");
  Sys.remove (fname ^ ".e");
  Sys.remove (fname ^ ".s");
  mesh

let write (type l) (mesh: l Mesh.t) file =
  match Mesh.layout mesh with
  | C_layout -> Mesh_easymeshC.write mesh file
  | Fortran_layout -> Mesh_easymeshF.write mesh file
