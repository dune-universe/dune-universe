(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Johannes Kanig, Stephane Lescuyer                       *)
(*  Jean-Christophe Filliatre, Romain Bardou and Francois Bobot           *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(* This is code which has been taken from                                 *)
(*  Ocamlgraph: a generic graph library for OCaml                         *)
(*  Copyright (C) 2004-2008                                               *)
(*  Sylvain Conchon, Jean-Christophe Filliatre and Julien Signoles        *)
(* and has been modified since then by the Mlpost authors                 *)

(* Unionfind structure over tuples of ints. Representatives contain the average
 * of their class, so they are of type float * float *)

type elt = float * float

type inputelt = int * int

type t

val init : inputelt list -> t

val find : inputelt -> t -> elt

val union : inputelt -> inputelt -> t -> unit

(* merge two classes and compute new average *)
val fold_classes : (elt -> 'a -> 'a) -> 'a -> t -> 'a
