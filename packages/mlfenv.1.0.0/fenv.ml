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

type rounding_mode = TONEAREST | UPWARD | DOWNWARD | TOWARDZERO

external fegetround : unit -> rounding_mode = "caml_fegetround"

external fesetround : rounding_mode -> unit = "caml_fesetround"
