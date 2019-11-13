(* This file is part of mlfenv.

   mlfenv is free software: you can redistribute it and/or modify it
   under the terms of the GNU Lesser General Public License as
   published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   mlfenv is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with mlfenv. If not, see
   <http://www.gnu.org/licenses/>. *)

module F = Fenv
module P = Printf

let _ =
  match F.fegetround () with
  | F.TONEAREST  -> P.printf "FE_NEAREST"
  | F.UPWARD     -> P.printf "FE_UPWARD"
  | F.DOWNWARD   -> P.printf "FE_DOWNWARD"
  | F.TOWARDZERO -> P.printf "FE_TOWARDZERO"
