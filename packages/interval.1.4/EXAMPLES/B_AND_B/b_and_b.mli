(* Copyright 2004 Jean-Marc Alliot / Nicolas Durand

   This file is part of the ocaml branch-and-bound library.

   The ocaml branch-and-bound library is free software: you can
   redistribute it and/or modify it under the terms of the GNU Lesser
   General Public License as published by the Free Software
   Foundation, either version 3 of the License, or (at your option)
   any later version.

   The ocaml branch-and-bound library is distributed in the hope that
   it will be useful,but WITHOUT ANY WARRANTY; without even the
   implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
   PURPOSE.  See the GNU Lesser General Public License for more
   details.

   You should have received a copy of the GNU Lesser General Public
   License along with the ocaml branch-and-bound library.  If not, see
   <http://www.gnu.org/licenses/>.  *)

val branch_and_bound :
  (float array -> float) ->
  (Interval.t array -> Interval.t) ->
  Interval.t array ->
  float ->
  float -> Interval.t array * Interval.t * float array * float
