(* File: mesh_triangle.mli

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

(** Interface for the Triangle 2D mesh generator.

    {{:http://www.cs.cmu.edu/~quake/triangle.html}Triangle} is a
    two-dimensional quality mesh generator and delaunay triangulator
    which was awarded the 2003 Wilkinson Prize.

    Jonathan Richard Shewchuk, the author of triangle, would like
    that, if you use Triangle in a scientific publication, you
    acknowledgment it with the following citation: Jonathan Richard
    Shewchuk, ``Triangle: Engineering a 2D Quality Mesh Generator and
    Delaunay Triangulator,'' in Applied Computational Geometry:
    Towards Geometric Engineering (Ming C. Lin and Dinesh Manocha,
    editors), volume 1148 of Lecture Notes in Computer Science, pages
    203-222, Springer-Verlag, Berlin, May 1996.

    @author Christophe Troestler (Christophe.Troestler\@umons.ac.be)
*)

(** Planar Straight Line Graph datastructure ({!Mesh.pslg} enriched
    with methods specific to Triangle).  By default, creating an
    object from this class results in all methods being initialized
    with empty arrays. *)
class ['l] pslg : 'l Bigarray.layout ->
object
  inherit ['l] Mesh.pslg

  method point_attribute : 'l Mesh.mat
  (** A matrix of size [a * n] ([fortran_layout]) where [a >= 0] is the
      number of attributes per point and [n] is the number of points
      (may be [0] if [a = 0]).  The attributes are typically
      floating-point values of physical quantities (such as mass or
      conductivity) associated with the nodes. *)
end

val pslg : ?hole: 'l Mesh.mat -> ?region: 'l Mesh.mat ->
           ?point_marker: 'l Mesh.int_vec -> ?point_attribute: 'l Mesh.mat ->
           'l Mesh.mat ->
           ?segment_marker: 'l Mesh.int_vec ->
           'l Mesh.int_mat -> 'l pslg
(** [pslg point segment] creates a PSLG with the proper methods.
    The default values for unspecified values are empty arrays.
    @raise Invalid_argument if a size is incorrect. *)


(** Object describing various characteristics of a mesh ({!Mesh.t}
    enriched with methods specific to Triangle).  It can also be used
    to construct [t] values from Mesh.t with empty attributes matrices. *)
class ['l] t : 'l #Mesh.t ->
object
  inherit ['l] Mesh.t

  method point_attribute : 'l Mesh.mat
  (** A matrix of size [a * n] ([fortran_layout]) where [a >= 0] is the
      number of attributes per point and [n] is the number of points
      (may be [0] if [a = 0]). *)

  method triangle_attribute : 'l Mesh.mat
  (** A matrix of size [a * n] ([fortran_layout]) where [a >= 0] is the
      number of attributes per triangle and [n] is the number of
      triangles (may be [0] if [a = 0]). *)
end

val create :
  ?hole: 'l Mesh.mat -> ?region: 'l Mesh.mat ->
  ?point_marker: 'l Mesh.int_vec -> ?point_attribute: 'l Mesh.mat ->
  'l Mesh.mat ->
  ?segment_marker: 'l Mesh.int_vec -> ?segment:'l Mesh.int_mat ->
  ?neighbor: 'l Mesh.int_mat ->
  ?edge: 'l Mesh.int_mat -> ?edge_marker: 'l Mesh.int_vec ->
  ?triangle_attribute: 'l Mesh.mat ->
  'l Mesh.int_mat -> 'l t
(** [create point triangle] creates a mesh with the proper methods.
   The default values for unspecified values are empty arrays.
   @raise Invalid_argument if a size is incorrect. *)


(** Voronoi diagram ({!Mesh.voronoi} enriched with methods specific to
    Triangle). *)
class type ['l] voronoi =
object
  inherit ['l] Mesh.voronoi
  method point_attribute : 'l Mesh.mat
  (** A matrix of size [a * n] ([fortran_layout]) where [a] is the
      number of attributes per point and [n] is the number of points. *)
end

exception Invalid_argument of string
(** Exception raised by the functions of this module to indicate that
    an argument not respecting the specifications was given. *)

type triunsuitable =
    float -> float -> float -> float -> float -> float -> float -> bool
(** Type of functions used to determine whether or not a selected
    triangle is too big (and needs to be refined).  [triunsuitable x1
    y1 x2 y2 x3 y3 area] must return [true] if the triangle is too
    big. The arguments are as follow:
    - [x1] and [y1] are the X an Y coordinates of the triangle's origin vertex.
    - [x2] and [y2] are the X an Y coordinates of the triangle's
    destination vertex.
    - [x3] and [y3] are the X an Y coordinates of the triangle's apex vertex.
    - [area] is the area of the triangle.
 *)

val triangulate :
  ?delaunay:bool ->
  ?min_angle:float ->
  ?max_area:float ->
  ?region_area: bool ->
  ?max_steiner:int ->
  ?voronoi:bool ->
  ?edge:bool ->
  ?neighbor:bool ->
  ?subparam:bool ->
  ?triunsuitable:triunsuitable ->
  ?check_finite:bool ->
  ?debug:bool ->
  ?verbose:[ `V | `VV | `VVV ] ->
  'a pslg -> 'a t * 'a voronoi
(** [triangulate pslg] returns a triangulation and a possibly a
    Voronoi diagram of the domain described by [pslg].  If
    [pslg#segment] is empty, the convex hull of the set of points is
    used.  Note that the numbering of nodes returned by this function
    may be far from optimal for the FEM.  See {!Mesh.band}.

    Note that [#point_marker] are not propagated to points created on
    the segment.  You must set [#segment_marker] for that.  If you do
    not set [#segment_marker], only the external boundary points will
    receive a marker of 1, the points on internal boundaries will have
    the marker 0.

    If [#region] is non-empty, it will be used to assign an additional
    floating-point attribute to each triangle.  It will be written as
    the single attribute of [#triangle_attribute] matrix.

    [#point_attribute] are interpolated to all points.


    @param delaunay generates a truly Delaunay (not just constrained
    Delaunay) triangulation.  It usually increases the number of
    vertices and triangles.  Default: [true].

    @param min_angle requires that all angles of triangles have at
    least that value.  If [min_angle < 0] or [min_angle > 60], the
    default value of Triangle is used.  If the minimum angle is 28.6
    degrees or smaller, Triangle is mathematically guaranteed to
    terminate (assuming infinite precision arithmetic--Triangle may
    fail to terminate if you run out of precision).

    @param edge return the edges of the triangulation in [#edge].
    Default: [true].

    @param neighbor Outputs an array of triangles neighboring each
    triangle in [#neighbor].  The first neighbor [#neighbor.{1,t}] of
    triangle [t] is opposite the first corner [#triangle.{1,t}] of
    triangle [t], and so on.  Default: [false].

    @param max_area Imposes a maximum triangle area.

    @param region_area uses the maximum area specified for each region in
    [#region].  Default: [false] even if [#region] is a non-empty
    matrix (i.e. you have to enable it explicitly).

    @param max_steiner Specifies the maximum number of Steiner points
    (vertices that are not in the input, but are added to meet the
    constraints on minimum angle and maximum area).  The default is to
    allow an unlimited number.

    @param triunsuitable a routine used to determine whether is
    triangle needs to be further splitted.

    @param subparam Generates second-order subparametric elements with
    six nodes each.  Default: [false].

    @param check_finite checks that all points coordinate are finite.
    As non-finite coordinates (e.g. NaN) puzzle Triangle without
    making it fail, the default is [true].  Set it to [false] only if
    you are sure your coordinates are finite.

    @param debug if true, outputs some explanation of what Triangle is
    doing and some statistics.  Default: [true] as it can contain
    interesting information during program development. *)

val refine :
  ?delaunay:bool ->
  ?min_angle:float ->
  ?max_area:float ->
  ?max_steiner:int ->
  ?voronoi:bool ->
  ?edge:bool ->
  ?neighbor:bool ->
  ?subparam:bool ->
  ?triangle_area:'a Mesh.vec ->
  ?triunsuitable:triunsuitable ->
  ?check_finite:bool ->
  ?debug:bool ->
  ?verbose:[ `V | `VV | `VVV ] ->
  'a t -> 'a t * 'a voronoi
(** [refine mesh] returns a refined version of the [mesh].  The
    initial indices for the points are preserved, additional nodes are
    added at the end.  However, the refinement is not hierarchical:
    there is no guarantee that each output element is contained in a
    single input element.

    @param triangle_area allows to specify, for each triangle [i], a
    maximum area [triangle_area.{i}].  If both [max_area] and
    [triangle_area] are specified, [triangle_area] is used.

    For other parameters, see {!triangulate}. *)

val copy : 'l t -> 'l t
(** See {!Mesh.copy}. *)

val sub : 'l t -> ?pos:int -> int -> 'l t
(** [sub mesh ?pos len] returns a new mesh keeping only the points
    with indices between [pos] and [pos + len - 1] (included).  See
    {!Mesh.sub} for more information. *)

val permute_points : 'l t -> ?inv:bool -> 'l Mesh.int_vec ->'l t
(** [permute_points perm mesh], see {!Mesh.permute_points}.  This
    version also permutes the [#point_attribute] matrix. *)

val permute_triangles : 'l t -> ?inv:bool -> 'l Mesh.int_vec ->'l t
(** [permute_points perm mesh], see {!Mesh.permute_points}.  This
    version also permutes the [#point_attribute] matrix. *)
;;
