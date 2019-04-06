


(** Type for log levels. *)
type log_level = Easy_logging__.Easy_logging_types.level
val show_log_level : log_level -> string
val pp_log_level : Format.formatter -> log_level -> unit

(** Signature of Handlers modules. *)
module type HandlersT = Easy_logging__.Easy_logging_types.HandlersT

(** Makes a Logging module from a Handlers module. *)
module MakeLogging :
functor (H : Easy_logging__Easy_logging_types.HandlersT) ->
sig

  (** See {! Easy_logging.Logging.logger} for documentation *)
  class logger :
          ?parent:logger option ->
          string ->
          object
            val name : string
            val mutable level : log_level option
            val mutable handlers : H.t list 
            val parent : logger option
            val propagate : bool
            
              
            method add_handler : H.t -> unit
            method set_level : log_level  -> unit
            method get_handlers : H.t list
            method set_propagate : bool -> unit         

            method effective_level : log_level
            
                 
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
      
  val get_logger : string -> logger
  val make_logger : ?propagate:bool -> string -> log_level  -> H.desc list -> logger


end


(** Default implementation of a Handlers module. *)
module Default_handlers = Default_handlers

(** Default implementation of a Logging module. *)
module Logging :
sig
  class logger :
          ?parent:logger option ->
          string ->
          object

            (** {3 Attributes} *)
            
            (** Name of the logger:
 - can be displayed in log messages.
 - defines the logger place in the logging tree. *)
            val name : string

            (** Value used to filter log messages.*)
            val mutable level : log_level option
              
            (** Registered handlers for this logger. *)
            val mutable handlers : Default_handlers.t list 

            (** The optional parent of this logger⋅ *)
            val parent : logger option

            (** Whether messages to this logger are propagated to its ancestors' handlers.*)
            val propagate : bool
            
              
              
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

                 
            (** Sets the log level of the logger instance. *)    
            method set_level : log_level  -> unit
                 
            (** Adds a handler to the logger instance. *)
            method add_handler : Default_handlers.t -> unit

                 
            method get_handlers : Default_handlers.t list

            (** Returs this logger level if it is not [None], else searches amongst ancestors for the first defined level; returns [NoLevel] if no level can be found. *) 
            method effective_level : log_level

                 
            method set_propagate : bool -> unit
          end

  (** [make_logger name level handlers_descs] 
      creates a new logger instance from the given arguments,
      then register it internally, and returns it.  *)
  val make_logger :
     ?propagate:bool -> string -> log_level  -> Default_handlers.desc list -> logger

  (** Returns a registered logger by name. *)
  val get_logger : string -> logger

end
