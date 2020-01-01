(*
 * Copyright 2017-2019 Cedric LE MOIGNE, cedlemo@gmx.com
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

(** Function_info — Struct representing a function. *)

open Ctypes

(** Function_info represents a function, method or constructor. To find out
    what kind of entity a Function_info represents, call
    Function_info.get_flags.
    See also Callable_info for information on how to retreive arguments and
    other metadata. *)
type t
val functioninfo : t structure typ

(** Obtain the symbol of the function. The symbol is the name of the exported
    function, suitable to be used as an argument to g_module_symbol().*)
val get_symbol:
  t structure ptr -> string

(** Obtain the Function_infoFlags for the info . *)
val get_flags:
  t structure ptr -> Bindings.Function_info.flags list

(** Obtain the property associated with this Function_info. Only
    Function_info with the flag GIFunction.Is_getter or GIFunction.Is_setter
    have a property set. For other cases, NULL will be returned. *)
val get_property:
  t structure ptr -> Property_info.t structure ptr option

(** Obtain the virtual function associated with this Function_info. Only
    Function_info with the flag Wraps_vfunc has a virtual function set. For
    other cases, None will be returned.
    In order to avoid circular call graph between Function_info and VFunc_info,
    this function will return a Callable_info. It is upto the user to use
    VFunc_info.from_callableinfo in order to have the VFunc_info.*)
val get_vfunc:
  t structure ptr -> Callable_info.t structure ptr option

(*
  gboolean	g_function_info_invoke ()
  GQuark	g_invoke_error_quark ()
*)

(** Just cast OCaml Ctypes base info to function info. *)
val cast_from_baseinfo:
  Base_info.t structure ptr -> t structure ptr

(** Just cast OCaml Ctypes function info to base info *)
val cast_to_baseinfo:
  t structure ptr -> Base_info.t structure ptr

(** Add unref of the C underlying structure whith Gc.finalise. *)
val add_unref_finaliser:
  t structure ptr -> t structure ptr

(** Return a Function_info.t from a Base_info.t, the underlying C structure
    ref count is increased and the value is Gc.finalis"ed" with
    Base_info.baseinfo_unref. *)
val from_baseinfo:
  Base_info.t structure ptr -> t structure ptr

(** Return a Base_info.t from a Function_info, the underlying C structure
    ref count is increased and the value is Gc.finalis"ed" with
    Base_info.baseinfo_unref. *)
val to_baseinfo:
  t structure ptr -> Base_info.t structure ptr

(** Just cast OCaml Ctypes callable info to function info. *)
val cast_from_callableinfo:
  Callable_info.t structure ptr -> t structure ptr

(** Just cast OCaml Ctypes function info to callable info *)
val cast_to_callableinfo:
  t structure ptr -> Callable_info.t structure ptr

(** Return a Function_info.t from a Callable_info.t, the underlying C structure
    ref count is increased and the value is Gc.finalis"ed" with
    Base_info.baseinfo_unref. *)
val from_callableinfo:
  Callable_info.t structure ptr -> t structure ptr

(** Return a Callable_info.t from a Function_info, the underlying C structure
    ref count is increased and the value is Gc.finalis"ed" with
    Base_info.baseinfo_unref. *)
val to_callableinfo:
  t structure ptr -> Callable_info.t structure ptr
