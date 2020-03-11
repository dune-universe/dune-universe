(* Copyright (C) Helmut Brandl  <helmut dot brandl at gmx dot net>

   This file is distributed under the terms of the GNU General Public License
   version 2 (GPLv2) as published by the Free Software Foundation.
*)

open Term
open Support

val typed_term:   expression -> type_term -> Context.t -> info_term
val untyped_term: expression -> Context.t -> info_term
val boolean_term: expression -> Context.t -> info_term
val result_term:  expression -> Context.t -> term
val structured_assertion:
  entities list withinfo -> compound -> compound -> Context.t
  -> formals0 * formals0 * term withinfo list * term withinfo list
val case_variables: expression -> bool -> Context.t -> expression * int array
val case_pattern: expression -> type_term -> Context.t -> names * term
