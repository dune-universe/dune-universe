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

type t = Ctypes.point = { x : float; y : float }

val zero : t

val add : t -> t -> t

val sub : t -> t -> t

val mult : float -> t -> t

val div : t -> float -> t

val rotated : float -> t -> t

val transform : Ctypes.matrix -> t -> t

val swapmx : t -> t

val swapmy : t -> t

val sign : t -> t

val middle : t -> t -> t

val norm : t -> float

val norm2 : t -> float

val dist : t -> t -> float

val dist2 : t -> t -> float

val list_min_max : ('a -> t * t) -> 'a list -> t * t

val list_min_max_float :
  ('a -> float * float * float * float) ->
  'a list ->
  float * float * float * float

val opp : t -> t

val print : Format.formatter -> t -> unit

module Infix : sig
  val ( +/ ) : t -> t -> t

  val ( -/ ) : t -> t -> t

  val ( */ ) : float -> t -> t

  val ( // ) : t -> float -> t
end

val norm_infinity : t -> t -> t

val segment : float -> t -> t -> t
