(*
 * Ptset: Sets of integers implemented as Patricia trees.
 * Copyright (C) 2000 Jean-Christophe FILLIATRE
 * Copyright (C) 2006- Markus Mottl
 * 
 * This software is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License version 2, as published by the Free Software Foundation.
 * 
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * 
 * See the GNU Library General Public License version 2 for more details
 * (enclosed in the file LGPL).
 *)

(*s Sets of integers implemented as Patricia trees.  The following
    signature is exactly [Set.S with type elt = int], with the same
    specifications. This is a purely functional data-structure. The
    performances are always better than the standard library's module
    [Set], except for linear insertion (building a set by insertion of
    consecutive integers). *)

type t

type elt = int

val empty : t

val is_empty : t -> bool

val mem : elt -> t -> bool

val add : elt -> t -> t

val singleton : elt -> t

val remove : elt -> t -> t

val union : t -> t -> t

val subset : t -> t -> bool
val intersect : t -> t -> bool

val inter : t -> t -> t

val diff : t -> t -> t

val equal : t -> t -> bool

val compare : t -> t -> int

val elements : t -> elt list

val choose : t -> elt

val choose_opt : t -> elt option

val cardinal : t -> int

val iter : (elt -> unit) -> t -> unit

val map : (elt -> elt) -> t -> t

val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a

val for_all : (elt -> bool) -> t -> bool

val exists : (elt -> bool) -> t -> bool

val filter : (elt -> bool) -> t -> t

val partition : (elt -> bool) -> t -> t * t

val split : elt -> t -> t * bool * t

val find : elt -> t -> elt

val find_opt : elt -> t -> elt option

(*s Warning: [min_elt], [max_elt], [find_first], [find_last], and their
    optional counterparts are linear w.r.t. the size of the set.

    E.g. [min_elt t] is barely more efficient than [fold min t (choose t)].
*)

val min_elt : t -> elt

val min_elt_opt : t -> elt option

val max_elt : t -> elt

val max_elt_opt : t -> elt option

val find_first : (elt -> bool) -> t -> elt

val find_first_opt : (elt -> bool) -> t -> elt option

val find_last : (elt -> bool) -> t -> elt

val find_last_opt : (elt -> bool) -> t -> elt option

val of_list : elt list -> t
