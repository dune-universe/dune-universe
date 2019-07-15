

module E = Easy_logging
   
type log_level = E.log_level
                   [@@deriving show]
let log_level_of_string = E.log_level_of_string

let log_level_to_yojson lvl : Yojson.Safe.json =
  `String (E.show_log_level lvl)
let log_level_of_yojson lvl_json =
  match lvl_json with
  | `String lvl_str ->
     log_level_of_string lvl_str
  | _ -> Error ("Cannot decode "^ (Yojson.Safe.to_string lvl_json) ^" to log level")

module type HandlersT =
  sig
    include E.HandlersT

    val desc_of_yojson :  Yojson.Safe.json -> (desc,string) result
    val desc_to_yojson : desc -> Yojson.Safe.json
    type config
       [@@deriving of_yojson]
    val default_config : config
    val set_config : config -> unit
  end


module Handlers =
  struct
    
    include E.Handlers
          
    type file_handlers_config_ = file_handlers_config =
      { logs_folder: string; [@default file_handlers_defaults.logs_folder]
        truncate: bool; [@default file_handlers_defaults.truncate]
        file_perms: int [@default file_handlers_defaults.file_perms]
      }
        [@@deriving yojson]

    let file_handlers_config_to_yojson = file_handlers_config__to_yojson
    let file_handlers_config_of_yojson = file_handlers_config__of_yojson
      
    type config_ = E.Handlers.config
      = {mutable file_handlers: file_handlers_config}
         [@@deriving yojson]

    let config_to_yojson = config__to_yojson
    let config_of_yojson = config__of_yojson                         
                               
    let default_config = {file_handlers = file_handlers_defaults}
                              
      
    let set_config c = config.file_handlers <- c.file_handlers
    type cli_json_params = {level : log_level}
        [@@deriving yojson]
    type cli_json_desc =  {cli : cli_json_params}
        [@@deriving yojson]
    type file_json_desc_params = {filename : string;level: log_level}
        [@@deriving yojson]
    type file_json_desc = {file : file_json_desc_params}
        [@@deriving yojson] 
      
    let desc_of_yojson json =
      match cli_json_desc_of_yojson json with
      | Ok {cli={level}} -> Ok (Cli level)
      | Error _ ->
         match file_json_desc_of_yojson json with
         | Ok {file={filename;level}} ->
            Ok (File (filename, level))
         | Error r -> Error ("desc_of yojson: "^r)
                    
    let desc_to_yojson d =
      match d with
      | Cli level -> cli_json_desc_to_yojson {cli={level}}
      | File (fname, lvl) ->
         file_json_desc_to_yojson
           {file= {filename=fname;level=lvl}}
        
  end           

module MakeLogging (H : HandlersT) =
  struct
    module L =  E.MakeLogging(H)
    include L

    type config_logger = {
        name: string;
        level : log_level; [@default NoLevel]
        handlers : H.desc list; [@default [] ]
        propagate : bool; [@default true]
      } [@@deriving of_yojson]

                       
    type config = {
        handlers : H.config; [@default H.default_config]
        loggers : config_logger list
      } [@@deriving of_yojson]

    let load_config config_json =
      match config_of_yojson config_json with
      | Ok {handlers;loggers} ->
         H.set_config handlers;
         List.iter (fun {name=name;
                         level=level;
                         handlers=handlers;
                         propagate=propagate} ->
             let l = make_logger name level handlers in
             l#set_propagate propagate) loggers
      | Error r ->
         failwith @@ "Error loading log config : "^r
        
    let load_config_str config_str =
      load_config (Yojson.Safe.from_string config_str)

    let load_config_file config_file =
      load_config (Yojson.Safe.from_file config_file)
  end

module Logging = MakeLogging(Handlers)
     
