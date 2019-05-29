open Ppxlib

let _prefix_from_enum_name = function
  | "t" -> ""
  | s -> s ^ "_"

let to_string_function_name ~enum_name = (_prefix_from_enum_name enum_name) ^ "to_string"
let from_string_function_name ~enum_name = (_prefix_from_enum_name enum_name) ^ "from_string"
let from_string_exn_function_name ~enum_name = (_prefix_from_enum_name enum_name) ^ "from_string_exn"

let constructor_is_bare constructor =
  match constructor with
  | {pcd_args = Pcstr_tuple []; pcd_res = None; _} -> true
  | _ -> false

let constructors_are_bare constructors =
  List.for_all constructor_is_bare constructors

