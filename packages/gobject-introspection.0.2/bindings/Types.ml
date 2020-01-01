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

(** The type of array in a GITypeInfo. *)
type array_type =
  | C          (** a C array, char[] for instance *)
  | Array      (** a GArray array *)
  | Ptr_array  (** a GPtrArray array *)
  | Byte_array (** a GByteArray array *)

let string_of_array_type = function
  | C -> "C"
  | Array -> "Array"
  | Ptr_array -> "Ptr_array"
  | Byte_array -> "Byte_array"

(** The type tag of a Type_info. *)
type tag =
  | Void (** void *)
  | Boolean (** boolean *)
  | Int8 (** 8-bit signed integer *)
  | Uint8 (** 8-bit unsigned integer *)
  | Int16 (** 16-bit signed integer *)
  | Uint16 (** 16-bit unsigned integer *)
  | Int32 (** 32-bit signed integer *)
  | Uint32 (** 32-bit unsigned integer *)
  | Int64 (** 64-bit signed integer *)
  | Uint64 (** 64-bit unsigned integer *)
  | Float (** float *)
  | Double (** double floating point *)
  | GType (** a GType *)
  | Utf8 (** a UTF-8 encoded string *)
  | Filename (** a filename, encoded in the same encoding as the native filesystem is using. *)
  | Array (** an array *)
  | Interface (** an extended interface object *)
  | GList (** a GList *)
  | GSList (** a GSList *)
  | GHash (** a GHashTable *)
  | Error (** a GError *)
  | Unichar (** Unicode character *)

let string_of_tag = function
  | Void -> "Void"
  | Boolean -> "Boolean"
  | Int8 -> "Int8"
  | Uint8 -> "Uint8"
  | Int16 -> "Int16"
  | Uint16 -> "Uint16"
  | Int32 -> "Int32"
  | Uint32 -> "Uint32"
  | Int64 -> "Int64"
  | Uint64 -> "Uint64"
  | Float -> "Float"
  | Double -> "Double"
  | GType -> "GType"
  | Utf8 -> "Utf8"
  | Filename -> "Filename"
  | Array -> "Array"
  | Interface -> "Interface"
  | GList -> "GList"
  | GSList -> "GSList"
  | GHash -> "GHash"
  | Error -> "Error"
  | Unichar -> "Unichar"

module Enums = functor (T : Cstubs.Types.TYPE) -> struct
  let gi_type_tag_void = T.constant "GI_TYPE_TAG_VOID" T.int64_t
  let gi_type_tag_boolean = T.constant "GI_TYPE_TAG_BOOLEAN" T.int64_t
  let gi_type_tag_int8 = T.constant "GI_TYPE_TAG_INT8" T.int64_t
  let gi_type_tag_uint8 = T.constant "GI_TYPE_TAG_UINT8" T.int64_t
  let gi_type_tag_int16 = T.constant "GI_TYPE_TAG_INT16" T.int64_t
  let gi_type_tag_uint16 = T.constant "GI_TYPE_TAG_UINT16" T.int64_t
  let gi_type_tag_int32 = T.constant "GI_TYPE_TAG_INT32" T.int64_t
  let gi_type_tag_uint32 = T.constant "GI_TYPE_TAG_UINT32" T.int64_t
  let gi_type_tag_int64 = T.constant "GI_TYPE_TAG_INT64" T.int64_t
  let gi_type_tag_uint64 = T.constant "GI_TYPE_TAG_UINT64" T.int64_t
  let gi_type_tag_float = T.constant "GI_TYPE_TAG_FLOAT" T.int64_t
  let gi_type_tag_double = T.constant "GI_TYPE_TAG_DOUBLE" T.int64_t
  let gi_type_tag_gtype = T.constant "GI_TYPE_TAG_GTYPE" T.int64_t
  let gi_type_tag_utf8 = T.constant "GI_TYPE_TAG_UTF8" T.int64_t
  let gi_type_tag_filename = T.constant "GI_TYPE_TAG_FILENAME" T.int64_t
  let gi_type_tag_array = T.constant "GI_TYPE_TAG_ARRAY" T.int64_t
  let gi_type_tag_interface = T.constant "GI_TYPE_TAG_INTERFACE" T.int64_t
  let gi_type_tag_glist = T.constant "GI_TYPE_TAG_GLIST" T.int64_t
  let gi_type_tag_gslist = T.constant "GI_TYPE_TAG_GSLIST" T.int64_t
  let gi_type_tag_ghash = T.constant "GI_TYPE_TAG_GHASH" T.int64_t
  let gi_type_tag_error = T.constant "GI_TYPE_TAG_ERROR" T.int64_t
  let gi_type_tag_unichar = T.constant "GI_TYPE_TAG_UNICHAR" T.int64_t

  let tag = T.enum "GITypeTag" ~typedef:true [
      Void, gi_type_tag_void;
      Boolean, gi_type_tag_boolean;
      Int8, gi_type_tag_int8;
      Uint8, gi_type_tag_uint8;
      Int16, gi_type_tag_int16;
      Uint16, gi_type_tag_uint16;
      Int32, gi_type_tag_int32;
      Uint32, gi_type_tag_uint32;
      Int64, gi_type_tag_int64;
      Uint64, gi_type_tag_uint64;
      Float, gi_type_tag_float;
      Double, gi_type_tag_double;
      GType, gi_type_tag_gtype;
      Utf8, gi_type_tag_utf8;
      Filename, gi_type_tag_filename;
      Array, gi_type_tag_array;
      Interface, gi_type_tag_interface;
      GList, gi_type_tag_glist;
      GSList, gi_type_tag_gslist;
      GHash, gi_type_tag_ghash;
      Error, gi_type_tag_error;
      Unichar, gi_type_tag_unichar;
    ]
      ~unexpected:(Utils.unexpected_value_for "GITypeTag")

  let gi_array_type_c = T.constant "GI_ARRAY_TYPE_C" T.int64_t
  let gi_array_type_array = T.constant "GI_ARRAY_TYPE_ARRAY" T.int64_t
  let gi_array_type_ptr_array = T.constant "GI_ARRAY_TYPE_PTR_ARRAY" T.int64_t
  let gi_array_type_byte_array = T.constant "GI_ARRAY_TYPE_BYTE_ARRAY" T.int64_t

  let array_type = T.enum "GIArrayType" ~typedef:true [
      C, gi_array_type_c;
      Array,  gi_array_type_array;
      Ptr_array, gi_array_type_ptr_array;
      Byte_array, gi_array_type_byte_array;
    ]
      ~unexpected:(Utils.unexpected_value_for "GIArrayType")
end
