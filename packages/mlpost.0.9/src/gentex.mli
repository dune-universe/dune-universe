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

val set_verbosity : bool -> unit

type t = {
  tex : Dviinterp.page;
  trans : Matrix.t;
  bb : float * float * float * float;
}

val create : string -> t

val get_dimen_pt : t -> float * float * float * float

val get_dimen_cm : t -> float * float * float * float

val bounding_box : t -> Point_lib.t * Point_lib.t

val get_bases_pt : t -> float list

val get_bases_cm : t -> float list

val print : Format.formatter -> t -> unit

val deb_print : Format.formatter -> t -> unit
(** donne la dimension en centimètre *)
