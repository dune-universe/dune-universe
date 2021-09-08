(** This module implements a warning management module *)

type warning =
   | Config of config_warning
and config_warning =
  | Missing_key of (string * string list * string) (* Aimed at providing info
                                             about incorrect json
                                             config file. The string
                                             list is a path to the
                                             group of the expected key
                                                    *)
  | Missing_name of (string * string list * string * string)
  | Missing_engine of (string * string list * string * string)
  | Default_engines
  | Default_colors
  | Bad_group of (string * string list * string * Yojson.Basic.t * string * string)
  | Json_error of string

val issue_warning : warning -> unit
