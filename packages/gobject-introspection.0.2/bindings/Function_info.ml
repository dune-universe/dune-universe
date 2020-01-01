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

(** Flags for a Function_info struct. *)
type flags =
  | Is_method      (** is a method. *)
  | Is_constructor (** is a constructor. *)
  | Is_getter      (** is a getter of a Property_info. *)
  | Is_setter      (** is a setter of a Property_info. *)
  | Wraps_vfunc    (** represents a virtual function. *)
  | Throws         (** the function may throw an error. *)

let string_of_flag = function
  | Is_method -> "Is_method"
  | Is_constructor -> "Is_constructor"
  | Is_getter -> "Is_getter"
  | Is_setter -> "Is_setter"
  | Wraps_vfunc -> "Wraps_vfunc"
  | Throws -> "Throws"

module Flags = functor (T : Cstubs.Types.TYPE) -> struct
  let gi_function_is_method = T.constant "GI_FUNCTION_IS_METHOD" T.int64_t
  let gi_function_is_constructor = T.constant "GI_FUNCTION_IS_CONSTRUCTOR" T.int64_t
  let gi_function_is_getter = T.constant "GI_FUNCTION_IS_GETTER" T.int64_t
  let gi_function_is_setter = T.constant "GI_FUNCTION_IS_SETTER" T.int64_t
  let gi_function_wraps_vfunc = T.constant "GI_FUNCTION_WRAPS_VFUNC" T.int64_t
  let gi_function_throws = T.constant "GI_FUNCTION_THROWS" T.int64_t
  let flags = T.enum "GIFunctionInfoFlags" ~typedef:true [] ~unexpected:(fun x -> x)
end
