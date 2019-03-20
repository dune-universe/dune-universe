(* File: min1D.mli

   Copyright (C) 2011

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: http://math.umons.ac.be/anum/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License version 3 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)

(** One dimensional minimization routines. *)

val brent : ?tol:float -> (float -> float) -> float -> float -> float * float
(** [brent f a b] uses Brent's method to return [(x, f x)] where [x] is
    the best approximation to a minimum point of [f] on the interval
    [[a,b]].

    @param tol Default: [sqrt epsilon_float].  The algorithm reduces the
    interval [[a,b]] containing an approximation [x] of the minimum
    until |x - (a+b)/2| + |b-a|/2 <= 2 sqrt epsilon_float |x| + tol / 3.

    Algorithm: G.Forsythe, M.Malcolm, C.Moler, Computer methods for
    mathematical computations. M., Mir, 1980. *)

val golden_search :
  ?tol:float -> (float -> float) -> float -> float -> float -> float * float
(** [golden_search f a b c] returns [(x, f x)] where [x] is an
    approximaion of a minimum point of [f] inside the interval
    [[a,c]].  The point [b] must be between [a] and [c] and such that
    [f(b)] is lower than [f(a)] and [f(c)].

    @param tol Default: [sqrt epsilon_float].  The algorithm reduces the
    interval [[a,c]] until |a - c| <= tol (|a| + |c|). *)
;;
