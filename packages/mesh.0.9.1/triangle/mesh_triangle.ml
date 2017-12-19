(* File: mesh_triangle.ml

   Copyright (C) 2009-

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: http://math.umons.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License version 3 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)

open Bigarray
open Mesh_utils

external init : unit -> unit = "ocaml_triangle_init"
let () = init()

include Mesh_triangle_common

let pslg ?(hole: 'a Mesh.mat option) ?(region: 'a Mesh.mat option)
         ?(point_marker: 'a Mesh.int_vec option)
         ?(point_attribute: 'a Mesh.mat option)
         (point: 'a Mesh.mat)
         ?(segment_marker: 'a Mesh.int_vec option) (segment: 'a Mesh.int_mat) =
  if Mesh_utils.is_c_layout (Array2.layout point) then
    let m = Mesh_triangleC.pslg
              ~hole:(mat_opt_to_c hole)
              ~region:(mat_opt_to_c region)
              ~point_attribute:(mat_opt_to_c point_attribute)
              ~point_marker:(vec_opt_to_c point_marker)
              ~point:(mat_to_c point)
              ~segment_marker:(vec_opt_to_c segment_marker)
              ~segment:(mat_to_c segment) in
    (Obj.magic (m: c_layout pslg) : 'a pslg)
  else
    let m = Mesh_triangleF.pslg
              ~hole:(mat_opt_to_fortran hole)
              ~region:(mat_opt_to_fortran region)
              ~point_attribute:(mat_opt_to_fortran point_attribute)
              ~point_marker:(vec_opt_to_fortran point_marker)
              ~point:(mat_to_fortran point)
              ~segment_marker:(vec_opt_to_fortran segment_marker)
              ~segment:(mat_to_fortran segment) in
    (Obj.magic (m: fortran_layout pslg) : 'a pslg)

let create ?(hole: 'a Mesh.mat option) ?(region: 'a Mesh.mat option)
      ?(point_marker: 'a Mesh.int_vec option)
      ?(point_attribute: 'l Mesh.mat option)
      (point:'a Mesh.mat)
      ?(segment_marker: 'a Mesh.int_vec option) ?(segment: 'a Mesh.int_mat option)
      ?(neighbor: 'a Mesh.int_mat option)
      ?(edge: 'a Mesh.int_mat option) ?(edge_marker: 'a Mesh.int_vec option)
      ?(triangle_attribute: 'l Mesh.mat option)
      (triangle: 'a Mesh.int_mat) =
  if Mesh_utils.is_c_layout (Array2.layout point) then
    let m = Mesh_triangleC.create
              ~hole:(mat_opt_to_c hole)
              ~region:(mat_opt_to_c region)
              ~point_attribute:(mat_opt_to_c point_attribute)
              ~point_marker:(vec_opt_to_c point_marker)
              ~point:(mat_to_c point)
              ~segment_marker:(vec_opt_to_c segment_marker)
              ~segment:(mat_opt_to_c segment)
              ~neighbor:(mat_opt_to_c neighbor)
              ~edge:(mat_opt_to_c edge)
              ~edge_marker:(vec_opt_to_c edge_marker)
              ~triangle_attribute:(mat_opt_to_c triangle_attribute)
              ~triangle:(mat_to_c triangle) in
    (Obj.magic (m: c_layout t) : 'a t)
  else
    let m = Mesh_triangleF.create
              ~hole:(mat_opt_to_fortran hole)
              ~region:(mat_opt_to_fortran region)
              ~point_attribute:(mat_opt_to_fortran point_attribute)
              ~point_marker:(vec_opt_to_fortran point_marker)
              ~point:(mat_to_fortran point)
              ~segment_marker:(vec_opt_to_fortran segment_marker)
              ~segment:(mat_opt_to_fortran segment)
              ~neighbor:(mat_opt_to_fortran neighbor)
              ~edge:(mat_opt_to_fortran edge)
              ~edge_marker:(vec_opt_to_fortran edge_marker)
              ~triangle_attribute:(mat_opt_to_fortran triangle_attribute)
              ~triangle:(mat_to_fortran triangle) in
    (Obj.magic (m: fortran_layout t) : 'a t)

let triangle ?delaunay ?min_angle ?max_area ?region_area ?max_steiner
             ?voronoi ?edge ?neighbor ?subparam ?triangle_area
             ?check_finite ?debug ?verbose ?triunsuitable
             ~pslg ~refine mesh =
  let layout = Array2.layout mesh#point in
  if is_c_layout layout then
    let triangle_area = match triangle_area with
      | None -> None
      | Some v -> Some(vec_to_c v) in
    let res =
      Mesh_triangleC.triangulate
        ?delaunay
        ?min_angle ?max_area ?region_area ?max_steiner ?voronoi ?neighbor ?edge
        ?subparam ?triangle_area ?triunsuitable ?check_finite ?debug ?verbose
        ~pslg ~refine (mesh_to_c mesh) in
    (Obj.magic(res:c_layout t * c_layout voronoi) : 'a t * 'a voronoi)
  else
    let triangle_area = match triangle_area with
      | None -> None
      | Some v -> Some(vec_to_fortran v) in
    let res =
      Mesh_triangleF.triangulate
        ?delaunay
        ?min_angle ?max_area ?region_area ?max_steiner ?voronoi ?neighbor ?edge
        ?subparam ?triangle_area ?triunsuitable ?check_finite ?debug ?verbose
        ~pslg ~refine (mesh_to_fortran mesh) in
    (Obj.magic(res:fortran_layout t * fortran_layout voronoi) : 'a t * 'a voronoi)


let triangulate ?delaunay ?min_angle ?max_area ?region_area ?max_steiner
      ?voronoi ?edge ?neighbor ?subparam ?triunsuitable ?check_finite
      ?debug ?verbose
      pslg =
  let mesh = mesh_of_pslg pslg in
  triangle ?delaunay ?min_angle ?max_area ?region_area ?max_steiner ?voronoi
    ?edge ?neighbor ?subparam ?triunsuitable ?check_finite ?debug ?verbose
    ~pslg:true ~refine:false mesh

let refine ?delaunay ?min_angle ?max_area ?max_steiner
    ?voronoi ?edge ?neighbor ?subparam ?triangle_area ?triunsuitable
    ?check_finite ?debug ?verbose mesh =
  triangle ?delaunay ?min_angle ?max_area ?max_steiner ?voronoi
    ?edge ?neighbor ?subparam ?triangle_area ?triunsuitable ?check_finite
    ?debug ?verbose
    ~pslg:false ~refine:true mesh


let copy (mesh: 'l t) =
  extend_mesh (Mesh.copy (mesh :> _ Mesh.t))
              ~point_attribute: (copy_mat mesh#point_attribute)
              ~triangle_attribute: (copy_mat mesh#triangle_attribute)

let sub mesh ?pos len =
  mesh_transform mesh
                 (fun m -> Mesh_triangleC.sub m ?pos len)
                 (fun m -> Mesh_triangleF.sub m ?pos len)

let permute_points (mesh: 'l #t) ?(inv=false) perm : 'l t =
  mesh_transform
    mesh
    (fun m -> Mesh_triangleC.permute_points m ~inv (vec_to_c perm))
    (fun m -> Mesh_triangleF.permute_points m ~inv (vec_to_fortran perm))

let permute_triangles (mesh: 'l #t) ?(inv=false) perm : 'l t =
  mesh_transform
    mesh
    (fun m -> Mesh_triangleC.permute_triangles m ~inv (vec_to_c perm))
    (fun m -> Mesh_triangleF.permute_triangles m ~inv (vec_to_fortran perm))


(* Loading various formats *)



(* Save to triangle format *)
(* let save mesh filename =
 *   (\* .node file *\)
 *   let fh = open_out (filename ^ ".node") in
 *   close_out fh
 *   (\* .ele file *\)
 *   (\* .poly file *\)
 *   (\* .edge file *\)
 *   (\* .neigh file *\) *)
