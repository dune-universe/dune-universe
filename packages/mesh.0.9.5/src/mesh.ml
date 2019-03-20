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

include Mesh_common

let pslg (type l) ?(hole: l mat option) ?(region: l mat option)
         ?(point_marker: l int_vec option) (point: l mat)
         ?(segment_marker: l int_vec option) (segment: l int_mat) : l pslg =
  match Array2.layout point with
  | C_layout ->
     MeshC.pslg ~hole ~region  ~point_marker ~point  ~segment_marker ~segment
  | Fortran_layout ->
     MeshF.pslg ~hole ~region  ~point_marker ~point  ~segment_marker ~segment

let create (type l) ?(hole: l mat option) ?(region: l mat option)
      ?(point_marker: l int_vec option)  (point:l mat)
      ?(segment_marker: l int_vec option) ?(segment: l int_mat option)
      ?(neighbor: l int_mat option)
      ?(edge: l int_mat option) ?(edge_marker: l int_vec option)
      (triangle: l int_mat) : l t =
  match Array2.layout point with
  | C_layout ->
     MeshC.create ~hole ~region  ~point_marker ~point
       ~segment_marker ~segment  ~neighbor  ~edge ~edge_marker  ~triangle
  | Fortran_layout ->
     MeshF.create ~hole ~region  ~point_marker ~point
       ~segment_marker ~segment  ~neighbor  ~edge ~edge_marker  ~triangle

let copy_vec v =
  let v' = Array1.create (Array1.kind v) (Array1.layout v) (Array1.dim v) in
  Array1.blit v v';
  v'

let copy_mat m =
  let m' = Array2.create (Array2.kind m) (Array2.layout m)
                         (Array2.dim1 m) (Array2.dim2 m) in
  Array2.blit m m';
  m'

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


let sub (type l) (mesh: l t) ?pos len : l t =
  match layout mesh with
  | C_layout -> MeshC.sub mesh ?pos len
  | Fortran_layout -> MeshF.sub mesh ?pos len

let band_height_P1 (type l) ?filter (mesh: l #t) =
  let mesh = (mesh :> l t) in
  match layout mesh with
  | C_layout -> MeshC.band_height_P1 filter mesh
  | Fortran_layout -> MeshF.band_height_P1 filter mesh

let cuthill_mckee (type l) ?(rev=true) ?(perm: l int_vec option)
      (mesh: l t) : l t =
  match layout mesh with
  | C_layout -> MeshC.cuthill_mckee ~rev perm mesh
  | Fortran_layout -> MeshF.cuthill_mckee ~rev perm mesh

let permute_points (type l) (mesh: l t) ?(inv=false) (perm: l int_vec) : l t =
  match layout mesh with
  | C_layout -> MeshC.permute_points mesh ~inv perm
  | Fortran_layout -> MeshF.permute_points mesh ~inv perm

let permute_triangles (type l) (mesh: l t) ?(inv=false)
      (perm: l int_vec) : l t =
  match layout mesh with
  | C_layout -> MeshC.permute_triangles mesh ~inv perm
  | Fortran_layout -> MeshF.permute_triangles mesh ~inv perm


module LaTeX =
struct
  type color = int

  let save (type l) ?edge (mesh: l #t) filename =
    let mesh = (mesh :> l t) in
    match layout mesh with
    | C_layout -> MeshC.latex ?edge mesh filename
    | Fortran_layout -> MeshF.latex ?edge mesh filename

  let write (type l) ?edge (mesh: l #t) fh =
    let mesh = (mesh :> l t) in
    match layout mesh with
    | C_layout -> MeshC.latex_write ?edge mesh fh
    | Fortran_layout -> MeshF.latex_write ?edge mesh fh

  let level_curves (type l) ?boundary (mesh: l #t) (z: l vec)
        ?level_eq levels filename =
    let mesh = (mesh :> l t) in
    match layout mesh with
    | C_layout -> MeshC.level_curves ?boundary mesh z
                    ?level_eq levels filename
    | Fortran_layout -> MeshF.level_curves ?boundary mesh z
                          ?level_eq levels filename

  let super_level (type l) ?boundary (mesh: l #t) (z: l vec) level color
        filename =
    let mesh = (mesh :> l t) in
    match layout mesh with
    | C_layout ->
       MeshC.super_level ?boundary mesh z level color filename
    | Fortran_layout ->
       MeshF.super_level ?boundary mesh z level color filename

  let sub_level (type l) ?boundary (mesh: l #t) (z: l vec) level color
        filename =
    let mesh = (mesh :> l t) in
    match layout mesh with
    | C_layout -> MeshC.sub_level ?boundary mesh z level color filename
    | Fortran_layout -> MeshF.sub_level ?boundary mesh z level color filename
end

let scilab (type l) (mesh: l #t) ?longitude ?azimuth ?mode ?box ?edgecolor
      (z: l vec) filename =
  let mesh = (mesh :> l t) in
  match layout mesh with
  | C_layout -> MeshC.scilab mesh ?longitude ?azimuth ?mode ?box
                  ?edgecolor z filename
  | Fortran_layout -> MeshF.scilab mesh ?longitude ?azimuth ?mode ?box
                        ?edgecolor z filename

let matlab (type l) (mesh: l #t) ?edgecolor ?linestyle ?facealpha
      (z: l vec) filename =
  let mesh = (mesh :> l t) in
  match layout mesh with
  | C_layout -> MeshC.matlab mesh ?edgecolor ?linestyle ?facealpha
                  z filename
  | Fortran_layout -> MeshF.matlab mesh ?edgecolor ?linestyle ?facealpha
                        z filename

let mathematica (type l) (mesh: l #t) (z: l vec) filename =
  let mesh = (mesh :> l t) in
  match layout mesh with
  | C_layout -> MeshC.mathematica mesh z filename
  | Fortran_layout -> MeshF.mathematica mesh z filename


(* Local Variables: *)
(* compile-command: "make -k -w -C .." *)
(* End: *)
