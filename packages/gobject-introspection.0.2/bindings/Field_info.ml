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

(** Flags for a Field_info. *)
type flags =
  | Is_readable (** field is readable. *)
  | Is_writable (** field is writable. *)

let string_of_flag = function
  | Is_readable -> "Is_readable"
  | Is_writable -> "Is_writable"

module Flags = functor (T : Cstubs.Types.TYPE) -> struct
  let gi_field_is_readable = T.constant "GI_FIELD_IS_READABLE" T.int64_t
  let gi_field_is_writable = T.constant "GI_FIELD_IS_WRITABLE" T.int64_t

  let flags = T.enum "GIFieldInfoFlags" ~typedef:true [] ~unexpected:(fun x -> x)
end
