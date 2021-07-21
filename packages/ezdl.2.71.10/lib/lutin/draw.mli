(*-----------------------------------------------------------------------
** Copyright (C) - Verimag.
** This file may only be copied under the terms of the CeCill
** Public License
**-----------------------------------------------------------------------
**
** File: draw.mli
** Main author: erwan.jahier@univ-grenoble-alpes.fr
*)

(** Draw values inside in a polyhedron and ranges. *)


(* XXX Ce module est vraiment de la cuisine !! *)

(*-----------------------------------------------------------------------*)
(** Drawing functions *)

val inside : int -> Store.t' -> Store.p -> int -> Var.num_subst list ->
  Var.num_subst list list
  (** [draw_inside s n ] draws points fairly inside the (bounded)
    solution hull contained in [s]. 

    Raises [No_numeric_solution] if no point is found
*)

val edges : int -> Store.t' -> Store.p -> int -> Var.num_subst list ->
  Var.num_subst list list
  (** Draw inside, but a little bit more at edges and vertices.

    Raises [No_numeric_solution] if no point is found
  *)

  (** [draw_edge s n] draws fairly on the edge of the
     (bounded) solution hull contained in [s]. 
  *)

val vertices : int -> Store.t' -> Store.p -> int -> Var.num_subst list ->
  Var.num_subst list list
  (** [draw_vertices s n] draws  among the vertices of
    the (bounded) solution hull contained in [s]. 

    Raises [No_numeric_solution] if no point is found
*)

val get_all_vertices : Store.t' -> Store.p -> Var.num_subst list ->
  Var.num_subst list list
  (** returns the solutions corresponding to the vertices of the solution
    hull contained in [s]. 

    Raises [No_numeric_solution] if no point is found
*)
