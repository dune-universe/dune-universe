(* easymesh.mli

  Copyright (C) 2001-

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: http://math.umons.ac.be/an/en/software/

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details; it is available at
   <http://www.fsf.org/copyleft/gpl.html>, or by writing to the Free
   Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
   02111-1307, USA.
*)
(**
   OCaml interface to EasyMesh.

   {{:http://www-dinma.univ.trieste.it/nirftc/research/easymesh/}EasyMesh}
   is a simple and easy to use mesh generator.  However, it is not
   very robust and can segfault instead of reporting an error.

   @author Christophe Troestler (Christophe.Troestler\@umons.ac.be)
*)

class ['l] pslg : 'l Bigarray.layout -> ['l] Mesh.pslg
(** An alias for {!Mesh.pslg}.  (Use this if you wan to ba able to
    easily switch mesh generators as some may require objects with
    more methods.) *)

val pslg : ?hole: 'l Mesh.mat -> ?region: 'l Mesh.mat ->
           ?point_marker: 'l Mesh.int_vec -> 'l Mesh.mat ->
           ?segment_marker: 'l Mesh.int_vec -> 'l Mesh.int_mat
           -> 'l pslg
(** Same as {!Mesh.pslg}.  Duplicated here to ease the writing of code
    that is easily ported to various meshers. *)

val create : ?hole: 'l Mesh.mat -> ?region: 'l Mesh.mat ->
             ?point_marker: 'l Mesh.int_vec ->
             'l Mesh.mat ->
             ?segment_marker: 'l Mesh.int_vec -> ?segment:'l Mesh.int_mat ->
             ?neighbor: 'l Mesh.int_mat ->
             ?edge: 'l Mesh.int_mat -> ?edge_marker: 'l Mesh.int_vec ->
             'l Mesh.int_mat -> 'l Mesh.t
(** Same as {!Mesh.create}.  Duplicated here to ease the writing of code
    that is easily ported to various meshers. *)


val triangulate : max_area:float -> 'layout Mesh.pslg -> 'layout Mesh.t
(** [triangulate ~max_area pslg] returns a triangulation of the
    Planar Straight Line Graph [pslg] given by [pslg.Mesh.points]
    and [pslg.Mesh.segment].  BEWARE that for EasyMesh, the boundary
    must have a positive (counterclockwise) orientation, holes must
    be delimited by a negatively oriented paths.

    [pslg.Mesh.points_marker] and [pslg.Mesh.segment_marker] may be set.

    The returned mesh sets [point], [point_marker], [triangle],
    [neighbor] and should genrally set [edge] and [edge_marker].

    @param max_area is given as an indication to the algorithm and
    may not be respected. *)

val read : 'layout Bigarray.layout -> string -> 'layout Mesh.t
  (** [read layout file] reads the mesh described by the files [file].n,
      [file].e and [file].s into a Mesh.t structure.  Only the fact that
      the files are well formed is checked (various exceptions may be
      thrown), not the fact that the data describe a real mesh. *)

val write : 'layout Mesh.t -> string -> unit
(** [write mesh file] writes the [mesh] in the files [file].n,
    [file].e and [file].s in easymesh format. *)
