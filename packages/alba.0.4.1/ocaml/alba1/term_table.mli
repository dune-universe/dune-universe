(* Copyright (C) Helmut Brandl  <helmut dot brandl at gmx dot net>

   This file is distributed under the terms of the GNU General Public License
   version 2 (GPLv2) as published by the Free Software Foundation.
*)

open Term

type t

val empty: t

val count: t -> int

val terms: t -> (int*int*int*term) list

val find: term -> int -> int -> (int->int) -> t -> (int * Term_sub.t) list

val unify: term -> int -> (int->int) -> t -> (int * Term_sub.t) list

val unify_with:
    term -> int -> int -> int -> bool -> (int->int) -> t -> (int * Term_sub.t) list

val add: term -> int -> int -> int -> (int->int) -> t -> t

val unify0: term -> int -> t -> (int * Term_sub.t) list

val unify0_with: term -> int -> int -> t -> (int * Term_sub.t) list

val add0: term -> int -> int -> int -> t -> t


val filter: (int->bool) -> t -> t

val remove: int -> t -> t
    (** [remove i tab] removes the term with the index [i] from the table
        [tab] *)

val filter_and_remap: (int->bool) -> (int->int) -> t -> t
