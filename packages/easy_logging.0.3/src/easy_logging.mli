


(** Type for log levels. *)
type log_level = Easy_logging__.Easy_logging_types.level
[@@deriving show { with_path = false }]



               
(** Signature of Handlers modules. *)
module type HandlersT = Easy_logging__.Easy_logging_types.HandlersT

(** Makes a Logging module from a Handlers module. *)
module MakeLogging :
functor (H : Easy_logging__Easy_logging_types.HandlersT) ->
sig

  (** See {! Easy_logging.Logging.logger} for documentation *)
  class logger :
          string ->
          log_level  ->
          H.desc list ->
          object
            val mutable handlers : H.t list
            val mutable level : log_level 
            val name : string
            method add_handler : H.t -> unit
            method set_level : log_level  -> unit

                 
                 
            method flash :   'a. ?tags:H.tag list ->
                             ('a, unit, string, unit) format4 -> 'a
            method error :   'a. ?tags:H.tag list ->
                             ('a, unit, string, unit) format4 -> 'a
            method warning : 'a. ?tags:H.tag list ->
                             ('a, unit, string, unit) format4 -> 'a
            method info :    'a. ?tags:H.tag list ->
                             ('a, unit, string, unit) format4 -> 'a
            method debug :   'a. ?tags:H.tag list ->
                             ('a, unit, string, unit) format4 -> 'a
              
            method sdebug : ?tags:H.tag list -> string -> unit
            method serror : ?tags:H.tag list -> string -> unit
            method sflash : ?tags:H.tag list -> string -> unit
            method sinfo : ?tags:H.tag list -> string -> unit
            method swarning : ?tags:H.tag list -> string -> unit

            method ldebug : ?tags:H.tag list -> string lazy_t -> unit
            method lerror : ?tags:H.tag list -> string lazy_t -> unit
            method lflash : ?tags:H.tag list -> string lazy_t -> unit
            method linfo : ?tags:H.tag list -> string lazy_t -> unit
            method lwarning : ?tags:H.tag list -> string lazy_t -> unit
          end
  val _loggers : (string, logger) Hashtbl.t
  val set_level : string -> log_level  -> unit
  val get_logger : string -> logger
  val make_logger : string -> log_level  -> H.desc list -> logger
  val dummy : unit -> logger

end


(** Default implementation of a Handlers module. *)
module Default_handlers = Default_handlers

(** Default implementation of a Logging module. *)
module Logging :
sig
  (** {3 Attributes} *)
  class logger :
          string ->
          log_level  ->
          Default_handlers.desc list ->
          object

            (** Name of the logger, displayed in log messages *)
            val name : string

            (** Value used to filter log messages.*)
            val mutable level : log_level 
            (** {[type log_level = | Debug | Info | Warning | Error | Flash | NoLevel ]} *)

            (** Registered handlers for this logger *)
            val mutable handlers : Default_handlers.t list
                                     
              
            (** {3 Classic logging Methods}
Each of these methods takes an optional [tag list], then a set of parameters the way a printf function does. If the log level of the instance is low enough, a log item will be created theb passed to the handlers.

Example : 
{[logger#warning "Unexpected value: %s" (to_string my_value)]}
 *)
              
            method flash : 'a. ?tags:Default_handlers.tag list -> ('a, unit, string, unit) format4 -> 'a
            method error : 'a. ?tags:Default_handlers.tag list -> ('a, unit, string, unit) format4 -> 'a
            method warning : 'a. ?tags:Default_handlers.tag list -> ('a, unit, string, unit) format4 -> 'a
            method info : 'a. ?tags:Default_handlers.tag list -> ('a, unit, string, unit) format4 -> 'a
            method debug : 'a. ?tags:Default_handlers.tag list -> ('a, unit, string, unit) format4 -> 'a
                 
                 

                 
            (** {3 Lazy logging methods} 
Each of these methods takes a [string lazy_t] as an input (as well as the optional [tag list]. If the log level of the instance is low enough, the lazy value will forced into a [string], a log item will be created then passed to the handlers.

Example:
{[logger#ldebug (lazy (heavy_calculation () ))]}
*)

          
            method ldebug : ?tags:Default_handlers.tag list -> string lazy_t -> unit
            method linfo : ?tags:Default_handlers.tag list -> string lazy_t -> unit
            method lwarning : ?tags:Default_handlers.tag list -> string lazy_t -> unit
            method lerror : ?tags:Default_handlers.tag list -> string lazy_t -> unit
            method lflash : ?tags:Default_handlers.tag list -> string lazy_t -> unit


                 (** {3 String logging methods}
These methods take a simple string as input.*)
                 
            method sdebug : ?tags:Default_handlers.tag list -> string -> unit
            method serror : ?tags:Default_handlers.tag list -> string -> unit
            method sflash : ?tags:Default_handlers.tag list -> string -> unit
            method sinfo : ?tags:Default_handlers.tag list -> string -> unit
            method swarning : ?tags:Default_handlers.tag list -> string -> unit

                 
            (** {3 Other methods} *)

            (** Adds a handler to the logger instance. *)
            method add_handler : Default_handlers.t -> unit
            
            (** Sets the log level of the logger instance. *)                 
            method set_level : log_level  -> unit

                                                     (*
            method log_msg : log_level -> string -> unit
            method log_msg_lazy : log_level -> string lazy_t -> unit *)
          end

  (** [make_logger name level handlers_descs] 
      creates a new logger instance from the given arguments,
      then register it internally, and returns it.  *)
  val make_logger :
    string -> log_level  -> Default_handlers.desc list -> logger

  (** Internally registered loggers. *)
  val _loggers : (string, logger) Hashtbl.t


  (** [set_level prefix level] sets the level of all 
      registered loggers whose name begins by [prefix]
      to [level]. *)
  val set_level : string -> log_level  -> unit


  (** Returns a registered logger by name. *)
  val get_logger : string -> logger


  (** dummy logger : does nothing. *)
  val dummy : unit -> logger
end
