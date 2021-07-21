(*-----------------------------------------------------------------------
** Copyright (C) - Verimag.
** This file may only be copied under the terms of the CeCill
** Public License
**-----------------------------------------------------------------------
**
** File: thickness.ml
** Author: erwan.jahier@univ-grenoble-alpes.fr
*)

(** Defines thickness data type. The thickness states how many test
vectors are generated at each step. *)


(** {4 Boolean thickness:} *)

type all_formula = bool
(** If [true], all formula reachable from the current node should be tried.
  Otherwise, only one formula is drawn according to transition weigths.
 *)

type formula_draw_nb = int
(** Number of draw to perform for each formula. *)


type boolean = all_formula * formula_draw_nb
(** The Boolean thickness *)



(** {4 Numeric thickness:} *)

(** Number a draw to perform inside the convex hull of solutions
  for numeric variables. *)
type inside_nb = int

(** Number a draw to perform at polyhedron edges. *)
type edges_nb = int

(** Number a draw to perform at polyhedron vertices.  That number of
  draw can either be all of them, or at most a fiwed number.
*)
type vertices_nb = All | AtMost of int

type numeric = (inside_nb * edges_nb * vertices_nb)
(** The numeric thickness *)


(** {4 Thickness:} *)

(** It is made of a [boolean] and a [numeric] thickness. *)
type t = boolean * numeric

let vertices_to_string = function All -> "all" | AtMost i -> (string_of_int i)
let numeric_to_string (i1,i2,v) = 
  "inside=" ^ (string_of_int i1) ^ 
  "  edges=" ^ (string_of_int i2) ^
  "  vertices" ^ (vertices_to_string v)
let boolean_to_string (i1,i2) = (string_of_bool i1) ^ ":" ^ (string_of_int i2)
let to_string (bt, nt) = (boolean_to_string bt) ^ ":" ^ (numeric_to_string nt)
