(*
 * Copyright 2019 Cedric LE MOIGNE, cedlemo@gmx.com
 * This file is part of OCaml-GObject-Introspection.
 *
 * OCaml-GObject-Introspection is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 *
 * OCaml-GObject-Introspection is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with OCaml-GObject-Introspection.  If not, see <http://www.gnu.org/licenses/>.
 *)

type flags =
  | Must_chain_up     (** chains up to the parent type *)
  | Must_override     (** overrides *)
  | Must_not_override (** does not override *)
  | Throws            (** Includes  a GError *)

let string_of_flag = function
  | Must_chain_up -> "Must_chain_up"
  | Must_override -> "Must_override"
  | Must_not_override -> "Must_not_override"
  | Throws -> "Throws"

module Flags = functor (T : Cstubs.Types.TYPE) -> struct
  let gi_vfunc_must_chain_up = T.constant "GI_VFUNC_MUST_CHAIN_UP" T.int64_t
  let gi_vfunc_must_override = T.constant "GI_VFUNC_MUST_OVERRIDE" T.int64_t
  let gi_vfunc_must_not_override = T.constant "GI_VFUNC_MUST_NOT_OVERRIDE" T.int64_t
  let gi_vfunc_throws = T.constant "GI_VFUNC_THROWS" T.int64_t
  let flags = T.enum "GIVFuncInfoFlags" ~typedef:true [] ~unexpected:(fun x -> x)
end
