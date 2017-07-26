open Core

type json_type = Json_wheel_external.Json_type.json_type =
    Object of (string * json_type) list
  | Array of json_type list
  | String of string
  | Int of int
  | Float of float
  | Bool of bool
  | Null
[@@deriving compare, sexp, typerep, bin_io]

type t = json_type [@@deriving compare, sexp, typerep, bin_io]
include (Json_wheel_external.Json_type : (module type of Json_wheel_external.Json_type
                                           with type json_type := json_type
                                            and type t := t))
