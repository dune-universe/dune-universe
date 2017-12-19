(* File: mesh.ml

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


open Bigarray
open Mesh_utils

include Mesh_common

let pslg ?(hole: 'a mat option) ?(region: 'a mat option)
         ?(point_marker: 'a int_vec option) (point: 'a mat)
         ?(segment_marker: 'a int_vec option) (segment: 'a int_mat) =
  if Mesh_utils.is_c_layout (Array2.layout point) then
    let m = MeshC.pslg ~hole:(mat_opt_to_c hole)
                       ~region:(mat_opt_to_c region)
                       ~point_marker:(vec_opt_to_c point_marker)
                       ~point:(mat_to_c point)
                       ~segment_marker:(vec_opt_to_c segment_marker)
                       ~segment:(mat_to_c segment) in
    (Obj.magic (m: c_layout pslg) : 'a pslg)
  else
    let m = MeshF.pslg ~hole:(mat_opt_to_fortran hole)
                       ~region:(mat_opt_to_fortran region)
                       ~point_marker:(vec_opt_to_fortran point_marker)
                       ~point:(mat_to_fortran point)
                       ~segment_marker:(vec_opt_to_fortran segment_marker)
                       ~segment:(mat_to_fortran segment) in
    (Obj.magic (m: fortran_layout pslg) : 'a pslg)

let create ?(hole: 'a mat option) ?(region: 'a mat option)
      ?(point_marker: 'a int_vec option)  (point:'a mat)
      ?(segment_marker: 'a int_vec option) ?(segment: 'a int_mat option)
      ?(neighbor: 'a int_mat option)
      ?(edge: 'a int_mat option) ?(edge_marker: 'a int_vec option)
      (triangle: 'a int_mat) =
  if Mesh_utils.is_c_layout (Array2.layout point) then
    let m = MeshC.create
              ~hole:(mat_opt_to_c hole)
              ~region:(mat_opt_to_c region)
              ~point_marker:(vec_opt_to_c point_marker)
              ~point:(mat_to_c point)
              ~segment_marker:(vec_opt_to_c segment_marker)
              ~segment:(mat_opt_to_c segment)
              ~neighbor:(mat_opt_to_c neighbor)
              ~edge:(mat_opt_to_c edge)
              ~edge_marker:(vec_opt_to_c edge_marker)
              ~triangle:(mat_to_c triangle) in
    (Obj.magic (m: c_layout t) : 'a t)
  else
    let m = MeshF.create
              ~hole:(mat_opt_to_fortran hole)
              ~region:(mat_opt_to_fortran region)
              ~point_marker:(vec_opt_to_fortran point_marker)
              ~point:(mat_to_fortran point)
              ~segment_marker:(vec_opt_to_fortran segment_marker)
              ~segment:(mat_opt_to_fortran segment)
              ~neighbor:(mat_opt_to_fortran neighbor)
              ~edge:(mat_opt_to_fortran edge)
              ~edge_marker:(vec_opt_to_fortran edge_marker)
              ~triangle:(mat_to_fortran triangle) in
    (Obj.magic (m: fortran_layout t) : 'a t)

let copy (mesh: 'l t) =
  make_mesh
    ~point: (copy_mat mesh#point)
    ~point_marker: (copy_vec mesh#point_marker)
    ~segment: (copy_mat mesh#segment)
    ~segment_marker: (copy_vec mesh#segment_marker)
    ~hole: (copy_mat mesh#hole)
    ~region: (copy_mat mesh#region)
    ~triangle: (copy_mat mesh#triangle)
    ~neighbor: (copy_mat mesh#neighbor)
    ~edge: (copy_mat mesh#edge)
    ~edge_marker: (copy_vec mesh#edge_marker)


let sub mesh ?pos len =
  mesh_transform mesh
                 (fun m -> MeshC.sub m ?pos len)
                 (fun m -> MeshF.sub m ?pos len)

let band_height_P1 ?filter mesh =
  if is_c_layout mesh then
    MeshC.band_height_P1 filter (mesh_to_c mesh)
  else
    MeshF.band_height_P1 filter (mesh_to_fortran mesh)

let cuthill_mckee ?(rev=true) ?(perm: 'l int_vec option) (mesh: 'l #t) =
  mesh_transform mesh
                 (fun m -> MeshC.cuthill_mckee ~rev (vec_opt_to_c perm) m)
                 (fun m -> MeshF.cuthill_mckee ~rev (vec_opt_to_fortran perm) m)

let permute_points (mesh: 'l #t) ?(inv=false) (perm: 'l int_vec) =
  mesh_transform mesh
                 (fun m -> MeshC.permute_points m ~inv (vec_to_c perm))
                 (fun m -> MeshF.permute_points m ~inv (vec_to_fortran perm))

let permute_triangles (mesh: 'l #t) ?(inv=false) (perm: 'l int_vec) =
  mesh_transform mesh
                 (fun m -> MeshC.permute_triangles m ~inv (vec_to_c perm))
                 (fun m -> MeshF.permute_triangles m ~inv (vec_to_fortran perm))


module LaTeX =
struct
  type color = int

  let save ?edge (mesh: _ #t) filename =
    if is_c_layout(mesh :> _ pslg)
    then MeshC.latex ?edge (mesh_to_c mesh) filename
    else MeshF.latex ?edge (mesh_to_fortran mesh) filename

  let write ?edge (mesh: _ #t) fh =
    if is_c_layout(mesh :> _ pslg)
    then MeshC.latex_write ?edge (mesh_to_c mesh) fh
    else MeshF.latex_write ?edge (mesh_to_fortran mesh) fh

  let level_curves ?boundary (mesh: 'a #t) (z: 'a vec)
      ?level_eq levels filename =
    if is_c_layout(mesh :> _ pslg) then
      MeshC.level_curves ?boundary (mesh_to_c mesh) (vec_to_c z)
                         ?level_eq levels filename
    else
      MeshF.level_curves ?boundary (mesh_to_fortran mesh) (vec_to_fortran z)
                         ?level_eq levels filename

  let super_level ?boundary (mesh: 'a #t) (z: 'a vec) level color filename =
    if is_c_layout mesh then
      MeshC.super_level ?boundary (mesh_to_c mesh) (vec_to_c z)
                        level color filename
    else
      MeshF.super_level ?boundary (mesh_to_fortran mesh) (vec_to_fortran z)
                        level color filename

  let sub_level ?boundary (mesh: 'a #t) (z: 'a vec) level color filename =
    if is_c_layout mesh then
      MeshC.sub_level ?boundary (mesh_to_c mesh) (vec_to_c z)
                      level color filename
    else
      MeshF.sub_level ?boundary (mesh_to_fortran mesh) (vec_to_fortran z)
                      level color filename
end

let scilab (mesh: 'a #t) ?longitude ?azimuth ?mode ?box ?edgecolor
      (z: 'a vec) filename =
  if is_c_layout mesh
  then MeshC.scilab (mesh_to_c mesh) ?longitude ?azimuth ?mode ?box
         ?edgecolor (vec_to_c z) filename
  else MeshF.scilab (mesh_to_fortran mesh) ?longitude ?azimuth ?mode ?box
         ?edgecolor (vec_to_fortran z) filename

let matlab (mesh: 'a #t) ?edgecolor ?linestyle ?facealpha (z: 'a vec) filename =
  if is_c_layout mesh
  then MeshC.matlab (mesh_to_c mesh) ?edgecolor ?linestyle ?facealpha
                    (vec_to_c z) filename
  else MeshF.matlab (mesh_to_fortran mesh) ?edgecolor ?linestyle ?facealpha
                    (vec_to_fortran z) filename

let mathematica (mesh: 'a #t) (z: 'a vec) filename =
  if is_c_layout mesh
  then MeshC.mathematica (mesh_to_c mesh) (vec_to_c z) filename
  else MeshF.mathematica (mesh_to_fortran mesh) (vec_to_fortran z) filename


(* Local Variables: *)
(* compile-command: "make -k -w -C .." *)
(* End: *)
