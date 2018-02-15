(* File: mesh_triangle_common.ml

   Copyright (C) 2014

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

class ['l] pslg (layout: 'l layout) =
object
  inherit [_] Mesh.pslg layout
  method point_attribute = Array2.create float64 layout 0 0
end

class ['l] t (mesh: 'l #Mesh.t) =
  let layout = Mesh.layout mesh in
  let point = mesh#point in
  let point_marker = mesh#point_marker in
  let segment = mesh#segment in
  let segment_marker = mesh#segment_marker in
  let hole = mesh#hole in
  let region = mesh#region in
  let triangle = mesh#triangle in
  let neighbor = mesh#neighbor in
  let edge = mesh#edge in
  let edge_marker = mesh#edge_marker in
  let empty_attribute = Array2.create float64 layout 0 0 in
  object
    method point = point
    method point_marker = point_marker
    method segment = segment
    method segment_marker = segment_marker
    method hole = hole
    method region = region
    method triangle = triangle
    method neighbor = neighbor
    method edge = edge
    method edge_marker = edge_marker

    method point_attribute = empty_attribute
    method triangle_attribute = empty_attribute
  end

class type ['l] voronoi =
object
  inherit ['l] Mesh.voronoi
  method point_attribute : 'l Mesh.mat
end

type triunsuitable =
  float -> float -> float -> float -> float -> float -> float -> bool

let register_triunsuitable (f: triunsuitable) =
  Callback.register "triunsuitable_callback" f

exception Invalid_argument of string

let invalid_arg m = raise(Invalid_argument m)

let is_finite x = neg_infinity < x && x < infinity (* => is not NaN *)

let make_mesh ~point ~point_marker ~segment ~segment_marker ~hole ~region
              ~triangle ~neighbor ~edge ~edge_marker ~point_attribute
              ~triangle_attribute =
  (object
      method point = point
      method point_marker = point_marker
      method segment = segment
      method segment_marker = segment_marker
      method hole = hole
      method region = region
      method triangle = triangle
      method neighbor = neighbor
      method edge = edge
      method edge_marker = edge_marker
      method point_attribute = point_attribute
      method triangle_attribute = triangle_attribute
    end : _ t)

let mesh_of_pslg (pslg: 'a pslg) =
  let layout = Array2.layout pslg#point in
  let empty_int_mat : 'a Mesh.int_mat = Array2.create int layout 2 0 in
  make_mesh
    (* PSLG *)
    ~point: pslg#point
    ~point_marker: pslg#point_marker
    ~segment: pslg#segment
    ~segment_marker: pslg#segment_marker
    ~hole: pslg#hole
    ~region: pslg#region
    (* Mesh *)
    ~triangle: empty_int_mat
    ~neighbor: empty_int_mat
    ~edge: empty_int_mat
    ~edge_marker: (Array1.create int layout 0)
    (* Mesh_triangle *)
    ~point_attribute: pslg#point_attribute
    ~triangle_attribute: (Array2.create float64 layout 2 0)

let extend_mesh (mesh: _ Mesh.t) ~point_attribute ~triangle_attribute =
  make_mesh
    ~point: mesh#point
    ~point_marker: mesh#point_marker
    ~segment: mesh#segment
    ~segment_marker: mesh#segment_marker
    ~hole: mesh#hole
    ~region: mesh#region
    ~triangle: mesh#triangle
    ~neighbor: mesh#neighbor
    ~edge: mesh#edge
    ~edge_marker: mesh#edge_marker
    ~point_attribute
    ~triangle_attribute
