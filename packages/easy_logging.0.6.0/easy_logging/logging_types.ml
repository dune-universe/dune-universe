

(** Possible level of a log item. *)
type level =
  | Debug
  | Trace
  | Info
  | Warning
  | Error
  | Flash
  | NoLevel


let level_of_string lvl_string =
  match String.lowercase_ascii lvl_string with
  | "debug" -> Ok Debug
  | "trace" -> Ok Trace
  | "info" -> Ok Info
  | "warning" -> Ok Warning
  | "error" -> Ok Error
  | "flash" -> Ok Flash
  | "nolevel" -> Ok NoLevel
  | _ -> Error (lvl_string ^ " does not represent a valid log level")


let show_level lvl = match lvl with
  | Debug    -> "Debug"
  | Trace    -> "Trace"
  | Info     -> "Info"
  | Warning  -> "Warning"
  | Error    -> "Error"
  | Flash    -> "Flash"
  | NoLevel  -> "NoLevel"

let pp_level fmt lvl = Format.pp_print_string fmt (show_level lvl)

type log_item = {
  level : level;
  logger_name : string;
  msg : string;
  tags : string list;
  timestamp: float;
}


module type HandlersT =
sig
  (** Type of a handler *)
  type t
  (** Applies the handler to a [log_item] *)
  val apply : t -> log_item -> unit

  (** Type used to instantiate a handler*)
  type desc

  type config

  (** default configuration used to instantiate handlers*)
  val default_config : config

  (** Instantiates a handler *)
  val make : ?config:config -> desc -> t

end
