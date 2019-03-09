
(* This file is free software, part of OLinq. See file "license" for more details. *)

(** {1 Interface to Yojson}

    The type {!json} is basically {!Yojson.Safe.json}

    {b STATUS: EXPERIMENTAL} *)

type ('a, +'card) query = ('a, 'card) OLinq.t

(*$inject
  module L = OLinq
  *)

type json =
  [ `Assoc of (string * json) list
  | `Bool of bool
  | `Float of float
  | `Int of int
  | `Intlit of string
  | `List of json list
  | `Null
  | `String of string
  | `Tuple of json list
  | `Variant of string * json option
  ]

val as_list : (json, _) query -> (json list, [`Any]) query
val as_assoc : (json, _) query -> ((string * json) list, [`Any]) query
val as_bool : (json, _) query -> (bool, [`Any]) query
val as_int : (json, _) query -> (int, [`Any]) query
val as_int_lit : (json, _) query -> (string, [`Any]) query
val as_float : (json, _) query -> (float, [`Any]) query
val as_null : (json, _) query -> (unit, [`Any]) query
val as_string : (json, _) query -> (string, [`Any]) query
val as_tuple : (json, _) query -> (json list, [`Any]) query
val as_variant : (json, _) query -> (string * json option, [`Any]) query

val assoc : string -> (json, _) query -> (json, [`Any]) query
(** Assuming the json is an object, obtain the given key, otherwise empty *)

val enter_assoc : (json, [<`One | `AtMostOne]) query -> (string * json, [`Any]) query

val enter_list : (json, [>`One | `AtMostOne]) query -> (json, [`Any]) query
(** If the json is a list, enter each of its element *)

val enter_tuple : (json, [<`One | `AtMostOne]) query -> (json, [`Any]) query

val enter_tuple_index : (json, _) query -> (int * json, [`Any]) query

val map_list : (json -> (json, [`One]) query) -> json -> (json, [`One | `AtMostOne]) query
(** [map_list f j] maps [f] over each element of [j], if [j] is a list,
    and replaces each element with the resulting json value, and wraps those
    again into a json list *)

val map_assoc : (string -> json -> (json, [`One]) query) -> json -> (json, [`One | `AtMostOne]) query
(** [map_assoc f j] maps each value of [j], assuming [j] is an [`Assoc], and
    wraps again in an assoc. *)

