(* File: mesh_display.mli

   Copyright (C) 2006-

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: http://math.umons.ac.be/an/en/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License version 2.1 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)

(** Draw meshes and level curves in an OCaml Graphics window.

    @author Christophe Troestler <Christophe.Troestler\@umons.ac.be>
*)

val draw :
  ?width:int -> ?height:int -> ?color: int -> ?points: bool ->
  ?point_idx:(int -> unit) -> ?triangle_idx:(int -> unit) ->
  ?voronoi:'a Mesh.voronoi -> ?point_marker_color: int ->
  'a Mesh.t -> unit
  (** [draw mesh] display the mesh on the current OCaml Graphics
      window with the bottom left corner at the current position.

      @param width the width (in pixels) of the mesh image (default: 600).
      @param height the height (in pixels) of the mesh image (default: 600).
      @param color the color to draw the mesh (default: the foreground color).
      @param points whether to draw the points (default: [true]).
      @param point_idx a function to be executed after each point is
             drawn, the current position being the center of the
             node.  Default: do nothing.
      @param triangle_idx a function to be executed for each triangle,
             the current point being the incentre of the triangle
             (the common intersection of the three bisectrices).
             Default: do nothing.
      @param voronoi draw also the given voronoi diagram.
      @param point_marker_color trigger the display of the point
             markers with the color given (default: no markers). *)

val display :
  ?width:int -> ?height:int -> ?color: int -> ?points: bool ->
  ?point_idx:(int -> unit) -> ?triangle_idx:(int -> unit) ->
  ?voronoi:'a Mesh.voronoi -> ?point_marker_color: int ->
  'a Mesh.t -> unit
(** [display mesh] open an OCaml graphic window and draw the mesh on
    it.  To quit the graph and let the program continue, one must
    press 'q' or 'Q' or click on the graphic.  See
    {!Mesh_display.draw} for the meaning of the optional
    parameters. *)

val level_curves : ?width:int -> ?height:int ->
  ?boundary:(int -> Graphics.color option) -> 'l Mesh.t -> 'l Mesh.vec ->
  ?level_eq:(float -> float -> bool) -> (float * Graphics.color) list -> unit
(** [level_curves mesh z levels] display a graphics window with the
    requested level curves.  Each level is a couple [(l, c)] where [l]
    is the lavel value and [c] is the color to be used to display it.
    The bottom left corner of the mesh is placed at the current
    position.

    @param boundary specifies the color of the boundary edges given
    their marker value.  Returning [None] means that one does not
    want the border with that marker to be printed.

    @param level_eq an approximate equality for levels that are judged not
    to be distinguishable.  It is expected that [l1 = l2] implies
    [level_eq l1 l2].  This function is mainly used not to draw
    the boundary edges at levels given in [levels].  *)

val display_level_curves : ?width:int -> ?height:int ->
  ?boundary:(int -> Graphics.color option) -> 'l Mesh.t -> 'l Mesh.vec ->
  ?level_eq:(float -> float -> bool) -> (float * Graphics.color) list -> unit
(** [display_level_curves mesh z levels] is like [level_curves mesh z
    levels] but opens the graph and "hold" it until 'q', 'Q' or a
    mouse button is pressed. *)

val super_level : ?width:int -> ?height:int ->
  ?boundary:(int -> Graphics.color option) ->
  'l Mesh.t -> 'l Mesh.vec -> float -> Graphics.color -> unit
(** [super_level mesh z level color] fill the super-level \{ (x,y) |
    z(x,y) > l \} with [color].  *)

val sub_level : ?width:int -> ?height:int ->
  ?boundary:(int -> Graphics.color option) ->
  'l Mesh.t -> 'l Mesh.vec -> float -> Graphics.color -> unit
(** [sub_level mesh z level color] same as {!super_level} except that
    the sub-level \{ (x,y) | z(x,y) < l \} is drawn. *)
