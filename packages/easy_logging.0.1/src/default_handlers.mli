(** 

 - Cli handler: outputs colored messages to stdout 
   {[ let h = Default_handlers.make (Cli Debug) ]}
 - File handler : outputs messages to a given file
   {[ let h = Default_handlers.make (File ("filename", Debug)) ]}


 *)


(** handler type *)
type t =
  {
    mutable fmt : Easy_logging_types.log_formatter;
    mutable level : Easy_logging_types.level;
    output : out_channel;
  }

val outputs : (string, out_channel) Hashtbl.t

val apply : t -> Easy_logging_types.log_item -> unit

val make_cli_handler : Easy_logging_types.level -> t

val make_file_handler :
  Easy_logging_types.level -> string -> t

val set_level : t -> Easy_logging_types.level -> unit

val set_formatter :
  t -> Easy_logging_types.log_formatter -> unit

val handlers : (string, t) Hashtbl.t

val register_handler : string -> t -> unit
type desc  =
  Cli of Easy_logging_types.level
| File of string * Easy_logging_types.level
| Reg of string

val make : desc -> t

val handle_test : t -> Easy_logging_types.log_item -> unit
