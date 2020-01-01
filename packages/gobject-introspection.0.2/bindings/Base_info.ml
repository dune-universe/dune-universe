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

type info_type =
  | Invalid (** invalid type *)
  | Function (** function, see Function_info *)
  | Callback (** callback, see Function_info *)
  | Struct (** struct, see Struct_info *)
  | Boxed (** boxed, see Struct_info or Union_info *)
  | Enum (** enum, see Enum_info *)
  | Flags (** flags, see Enum_info *)
  | Object (** object, see Object_info *)
  | Interface (** interface, see Interface_info *)
  | Constant (** contant, see Constant_info *)
  | Invalid_0 (** deleted, used to be GI_INFO_TYPE_ERROR_DOMAIN. *)
  | Union (** union, see Union_info *)
  | Value (** enum value, see Value_info *)
  | Signal (** signal, see Signal_info *)
  | Vfunc (** virtual function, see VFunc_info *)
  | Property (** GObject property, see Property_info *)
  | Field (** struct or union field, see Field_info *)
  | Arg (** argument of a function or callback, see Arg_info *)
  | Type (** type information, see Type_info *)
  | Unresolved (** unresolved type, a type which is not present in the typelib, or any of its dependencies. *)

let string_of_info_type = function
  | Invalid -> "Invalid"
  | Function -> "Function"
  | Callback -> "Callback"
  | Struct -> "Struct"
  | Boxed -> "Boxed"
  | Enum -> "Enum"
  | Flags -> "Flags"
  | Object -> "Object"
  | Interface -> "Interface"
  | Constant -> "Constant"
  | Invalid_0 -> "Invalid_0"
  | Union -> "Union"
  | Value -> "Value"
  | Signal -> "Signal"
  | Vfunc -> "Vfunc"
  | Property -> "Property"
  | Field -> "Field"
  | Arg -> "Arg"
  | Type -> "Type"
  | Unresolved -> "Unresolved "

module Enums = functor (T : Cstubs.Types.TYPE) -> struct
  let gi_info_type_invalid = T.constant "GI_INFO_TYPE_INVALID" T.int64_t
  let gi_info_type_function = T.constant "GI_INFO_TYPE_FUNCTION" T.int64_t
  let gi_info_type_callback = T.constant "GI_INFO_TYPE_CALLBACK" T.int64_t
  let gi_info_type_struct = T.constant "GI_INFO_TYPE_STRUCT" T.int64_t
  let gi_info_type_boxed = T.constant "GI_INFO_TYPE_BOXED" T.int64_t
  let gi_info_type_enum = T.constant "GI_INFO_TYPE_ENUM" T.int64_t
  let gi_info_type_flags = T.constant "GI_INFO_TYPE_FLAGS" T.int64_t
  let gi_info_type_object = T.constant "GI_INFO_TYPE_OBJECT" T.int64_t
  let gi_info_type_interface = T.constant "GI_INFO_TYPE_INTERFACE" T.int64_t
  let gi_info_type_constant = T.constant "GI_INFO_TYPE_CONSTANT" T.int64_t
  let gi_info_type_invalid_0 = T.constant "GI_INFO_TYPE_INVALID_0" T.int64_t
  let gi_info_type_union = T.constant "GI_INFO_TYPE_UNION" T.int64_t
  let gi_info_type_value = T.constant "GI_INFO_TYPE_VALUE" T.int64_t
  let gi_info_type_signal = T.constant "GI_INFO_TYPE_SIGNAL" T.int64_t
  let gi_info_type_vfunc = T.constant "GI_INFO_TYPE_VFUNC" T.int64_t
  let gi_info_type_property = T.constant "GI_INFO_TYPE_PROPERTY" T.int64_t
  let gi_info_type_field = T.constant "GI_INFO_TYPE_FIELD" T.int64_t
  let gi_info_type_arg = T.constant "GI_INFO_TYPE_ARG" T.int64_t
  let gi_info_type_type = T.constant "GI_INFO_TYPE_TYPE" T.int64_t
  let gi_info_type_unresolved = T.constant "GI_INFO_TYPE_UNRESOLVED" T.int64_t

  let info_type = T.enum "GIInfoType" ~typedef:true [
      Invalid, gi_info_type_invalid;
      Function, gi_info_type_function;
      Callback, gi_info_type_callback;
      Struct, gi_info_type_struct;
      Boxed, gi_info_type_boxed;
      Enum, gi_info_type_enum;
      Flags, gi_info_type_flags;
      Object, gi_info_type_object;
      Interface, gi_info_type_interface;
      Constant, gi_info_type_constant;
      Invalid_0, gi_info_type_invalid_0;
      Union, gi_info_type_union;
      Value, gi_info_type_value;
      Signal, gi_info_type_signal;
      Vfunc, gi_info_type_vfunc;
      Property, gi_info_type_property;
      Field, gi_info_type_field;
      Arg, gi_info_type_arg;
      Type, gi_info_type_type;
      Unresolved, gi_info_type_unresolved;
    ]
      ~unexpected:(Utils.unexpected_value_for "GIInfoType")
end
