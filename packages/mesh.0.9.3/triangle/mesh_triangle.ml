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

external init : unit -> unit = "ocaml_triangle_init"
let () = init()

include Mesh_triangle_common

let pslg (type l) ?(hole: l Mesh.mat option) ?(region: l Mesh.mat option)
      ?(point_marker: l Mesh.int_vec option)
      ?(point_attribute: l Mesh.mat option)
      (point: l Mesh.mat)
      ?(segment_marker: l Mesh.int_vec option) (segment: l Mesh.int_mat)
    : l pslg =
  match Array2.layout point with
  | C_layout ->
     Mesh_triangleC.pslg ~hole ~region
       ~point_attribute ~point_marker ~point  ~segment_marker ~segment
  | Fortran_layout ->
     Mesh_triangleF.pslg ~hole ~region
       ~point_attribute ~point_marker ~point  ~segment_marker ~segment

let create (type l) ?(hole: l Mesh.mat option) ?(region: l Mesh.mat option)
      ?(point_marker: l Mesh.int_vec option)
      ?(point_attribute: l Mesh.mat option)
      (point:l Mesh.mat)
      ?(segment_marker: l Mesh.int_vec option) ?(segment: l Mesh.int_mat option)
      ?(neighbor: l Mesh.int_mat option)
      ?(edge: l Mesh.int_mat option) ?(edge_marker: l Mesh.int_vec option)
      ?(triangle_attribute: l Mesh.mat option)
      (triangle: l Mesh.int_mat) : l t =
  match Array2.layout point with
  | C_layout ->
     Mesh_triangleC.create ~hole ~region
       ~point_attribute ~point_marker ~point  ~segment_marker ~segment
       ~neighbor  ~edge ~edge_marker  ~triangle_attribute ~triangle
  | Fortran_layout ->
     Mesh_triangleF.create ~hole ~region
       ~point_attribute ~point_marker ~point  ~segment_marker ~segment
       ~neighbor  ~edge ~edge_marker  ~triangle_attribute ~triangle

let triangle (type l) ?delaunay ?min_angle ?max_area ?region_area ?max_steiner
      ?voronoi ?edge ?neighbor ?subparam
      ?(triangle_area: l Mesh.vec option)
      ?check_finite ?debug ?verbose ?triunsuitable
      ~pslg ~refine (mesh: l t) : l t * l voronoi =
  let layout = Mesh.layout mesh in
  match layout with
  | C_layout ->
    let triangle_area = match triangle_area with
      | None -> None
      | Some v -> Some v in
    Mesh_triangleC.triangulate
      ?delaunay
      ?min_angle ?max_area ?region_area ?max_steiner ?voronoi ?neighbor ?edge
      ?subparam ?triangle_area ?triunsuitable ?check_finite ?debug ?verbose
      ~pslg ~refine mesh
  | Fortran_layout ->
     let triangle_area = match triangle_area with
       | None -> None
       | Some v -> Some v in
     Mesh_triangleF.triangulate
        ?delaunay
        ?min_angle ?max_area ?region_area ?max_steiner ?voronoi ?neighbor ?edge
        ?subparam ?triangle_area ?triunsuitable ?check_finite ?debug ?verbose
        ~pslg ~refine mesh


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


let copy_mat m =
  let m' = Array2.create (Array2.kind m) (Array2.layout m)
                         (Array2.dim1 m) (Array2.dim2 m) in
  Array2.blit m m';
  m'

let copy (mesh: 'l t) =
  extend_mesh (Mesh.copy (mesh :> _ Mesh.t))
              ~point_attribute: (copy_mat mesh#point_attribute)
              ~triangle_attribute: (copy_mat mesh#triangle_attribute)

let sub (type l) (mesh: l t) ?pos len : l t =
  match Mesh.layout mesh with
  | C_layout -> Mesh_triangleC.sub mesh ?pos len
  | Fortran_layout -> Mesh_triangleF.sub mesh ?pos len

let permute_points (type l) (mesh: l t) ?(inv=false)
      (perm: l Mesh.int_vec) : l t =
  match Mesh.layout mesh with
  | C_layout -> Mesh_triangleC.permute_points mesh ~inv perm
  | Fortran_layout -> Mesh_triangleF.permute_points mesh ~inv perm

let permute_triangles (type l) (mesh: l t) ?(inv=false)
      (perm: l Mesh.int_vec) : l t =
  match Mesh.layout mesh with
  | C_layout -> Mesh_triangleC.permute_triangles mesh ~inv perm
  | Fortran_layout -> Mesh_triangleF.permute_triangles mesh ~inv perm


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
