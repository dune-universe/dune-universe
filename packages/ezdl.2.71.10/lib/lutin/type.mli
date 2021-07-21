(*-----------------------------------------------------------------------
** Copyright (C) - Verimag.
** This file may only be copied under the terms of the CeCill
** Public License
**-----------------------------------------------------------------------
**
** File: type.mli
** Author: erwan.jahier@univ-grenoble-alpes.fr
*)

type enum_value = string
type field = string

type t =
  | BoolT
  | IntT
  | FloatT
  | UT of structured
and
  structured =
  | ArrayT  of int * t
  | StructT of (field * t) list
  | EnumT   of enum_value list

val to_data_t : t -> Data.t

val to_string : t -> string
val to_string2 : t -> string
val to_string3 : t -> string

(* generated a string following the C syntax convention *)
val to_cstring : t -> string
val to_cstring_bis : t -> string

val structured_to_string : structured -> string
