
module E = Easy_logging


module Logging_types =
struct
  include E.Logging_internals.Logging_types

  let level_to_yojson lvl : Yojson.Safe.t =
    `String (show_level lvl)
  let level_of_yojson lvl_json =
    match lvl_json with
    | `String lvl_str ->
      level_of_string lvl_str
    | _ -> Error ("Cannot decode "^ (Yojson.Safe.to_string lvl_json) ^" to log level")
end

open Logging_types
module type HandlersT =
sig
  include HandlersT

  val desc_of_yojson :  Yojson.Safe.t -> (desc,string) result
  val desc_to_yojson : desc -> Yojson.Safe.t

  val config_of_yojson : Yojson.Safe.t -> (config,string) result
  val config_to_yojson : config -> Yojson.Safe.t
end


module Handlers =
struct

  include E.Handlers

  module FileHandlers = struct
    include E.Handlers.FileHandler
    type config_ = config =
      { logs_folder: string; [@default default_config.logs_folder]
        truncate: bool; [@default default_config.truncate]
        file_perms: int; [@default default_config.file_perms]
        date_prefix: string option; [@default default_config.date_prefix]
        versioning: int option; [@default default_config.versioning]
        suffix: string; [@default default_config.suffix]
      }
    [@@deriving yojson]

    let config_to_yojson = config__to_yojson
    let config_of_yojson = config__of_yojson
  end
  type config_ = E.Handlers.config
  = {file_handlers: FileHandlers.config}
  [@@deriving yojson]

  let config_to_yojson = config__to_yojson
  let config_of_yojson = config__of_yojson


  type cli_json_params = {level : level}
  [@@deriving yojson]
  type cli_json_desc =  {cli : cli_json_params}
  [@@deriving yojson]
  type cli_err_json_desc =  {cli_err : cli_json_params}
  [@@deriving yojson]
  type file_json_desc_params = {filename : string;level: level}
  [@@deriving yojson]
  type file_json_desc = {file : file_json_desc_params}
  [@@deriving yojson]

  let desc_of_yojson json =
    match cli_json_desc_of_yojson json with
    | Ok {cli={level}} -> Ok (Cli level)
    | Error _ ->
      match cli_err_json_desc_of_yojson json with
      | Ok {cli_err={level}} -> Ok (CliErr level)
      | Error _ ->
        match file_json_desc_of_yojson json with
        | Ok {file={filename;level}} ->
          Ok (File (filename, level))
        | Error r -> Error ("desc_of yojson: "^r)

  let desc_to_yojson d =
    match d with
    | Cli level -> cli_json_desc_to_yojson {cli={level}}
    | CliErr level -> cli_err_json_desc_to_yojson {cli_err={level}}
    | File (fname, lvl) ->
      file_json_desc_to_yojson
        {file= {filename=fname;level=lvl}}

end


module MakeLogging (H : HandlersT) =
struct
  include E.Logging_internals.MakeLogging(H)
  type config_logger = {
    name: string;
    level : level; [@default NoLevel]
    handlers : H.desc list; [@default [] ]
    propagate : bool; [@default true]
  } [@@deriving of_yojson]


  type global_config = {
    handlers_config : H.config; [@default H.default_config] [@key "handlers"]
    loggers_config : config_logger list [@key "loggers"]
  } [@@deriving of_yojson]

  let load_global_config config_json =
    match global_config_of_yojson config_json with
    | Ok {handlers_config;loggers_config} ->
      set_handlers_config handlers_config;
      List.iter (fun {name=name;
                      level=level;
                      handlers=handlers;
                      propagate=propagate} ->
                  let l = make_logger name level handlers in
                  l#set_propagate propagate) loggers_config
    | Error r ->
      failwith @@ "Error loading log config : "^r

  let load_global_config_str config_str =
    load_global_config (Yojson.Safe.from_string config_str)

  let load_global_config_file config_file =
    load_global_config (Yojson.Safe.from_file config_file)
end
