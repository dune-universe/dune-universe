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

let grp_preamble file path =
  match path with
  | [] -> Printf.sprintf "In file \"%s\"" file
  | path -> Printf.sprintf "In file \"%s\", under the path \"%s\"" file (Utils.string_of_list " -> " (fun x -> x) path)

let issue_warning = function
  | Config (Missing_key (file,path,key)) ->
     Logs.warn (fun m -> m "%s, key \"%s\" is missing" (grp_preamble file path) key)
  | Config (Missing_name (file,path,key,msg)) ->
     Logs.warn (fun m -> m "%s, key \"%s\" is missing in association with signature engine \"%s\"" (grp_preamble file path) key msg)
  | Config (Missing_engine (file,path,key,msg)) ->
     Logs.warn (fun m -> m "%s, key \"%s\" is missing in association with signature name \"%s\"" (grp_preamble file path) key msg)
  | Config Default_engines -> Logs.warn (fun m -> m "Using default signature to engine mapping")
  | Config Default_colors -> Logs.warn (fun m -> m "Using default bacground and node colors")
  | Config (Bad_group (file,path,yojson_msg,json,msg,msg')) ->
     let () = Logs.warn (fun m -> m "%s, %s" (grp_preamble file path) yojson_msg) in
     let () = Logs.warn (fun m -> m "%s, but got: \"%s\"" msg (Yojson.Basic.pretty_to_string ~std:true json)) in
     Logs.warn (fun m -> m "%s" msg')
  | Config (Json_error msg) -> 
     let () = Logs.warn (fun m -> m "Json error: %s" msg) in
     Logs.warn (fun m -> m "Using default configuration")
