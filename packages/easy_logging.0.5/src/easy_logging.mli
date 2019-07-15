


(** Type for log levels. *)
type log_level = Easy_logging__.Easy_logging_types.level
val show_log_level : log_level -> string
val pp_log_level : Format.formatter -> log_level -> unit
val log_level_of_string : string -> (log_level,string) result

(** Signature of Handlers modules. *)
module type HandlersT = Easy_logging__.Easy_logging_types.HandlersT

(** Makes a Logging module from a Handlers module. *)
module MakeLogging :
functor (H : Easy_logging__Easy_logging_types.HandlersT) ->
sig

  val debug : bool ref
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
            val mutable tag_generators : (unit -> H.tag) list
              
            method add_handler : H.t -> unit
            method set_level : log_level  -> unit
            method get_handlers : H.t list
            method set_propagate : bool -> unit         

            method effective_level : log_level
            method add_tag_generator : (unit -> H.tag) -> unit
                 
            method flash :   'a. ?tags:H.tag list ->
                             ('a, unit, string, unit) format4 -> 'a
            method error :   'a. ?tags:H.tag list ->
                             ('a, unit, string, unit) format4 -> 'a
            method warning : 'a. ?tags:H.tag list ->
                             ('a, unit, string, unit) format4 -> 'a
            method info :    'a. ?tags:H.tag list ->
                             ('a, unit, string, unit) format4 -> 'a
            method trace :    'a. ?tags:H.tag list ->
                             ('a, unit, string, unit) format4 -> 'a
            method debug :   'a. ?tags:H.tag list ->
                             ('a, unit, string, unit) format4 -> 'a

            method ldebug : ?tags:H.tag list -> string lazy_t -> unit
            method ltrace : ?tags:H.tag list -> string lazy_t -> unit
            method lerror : ?tags:H.tag list -> string lazy_t -> unit
            method lflash : ?tags:H.tag list -> string lazy_t -> unit
            method linfo : ?tags:H.tag list -> string lazy_t -> unit
            method lwarning : ?tags:H.tag list -> string lazy_t -> unit

                 
            method sdebug : ?tags:H.tag list -> string -> unit
            method strace : ?tags:H.tag list -> string -> unit
            method sinfo : ?tags:H.tag list -> string -> unit
            method swarning : ?tags:H.tag list -> string -> unit
            method serror : ?tags:H.tag list -> string -> unit
            method sflash : ?tags:H.tag list -> string -> unit
          end
      
  val get_logger : string -> logger
  val make_logger : ?propagate:bool -> string -> log_level  -> H.desc list -> logger


end


(** Default implementation of a Handlers module. *)
module Handlers = Handlers

(** Default implementation of a Logging module. *)
module Logging :
sig
  val debug : bool ref
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
            val mutable handlers : Handlers.t list 

            (** The optional parent of this logger. *)
            val parent : logger option

            (** Whether messages passed to this logger are propagated to its ancestors' handlers.*)
            val propagate : bool

            (** The list of functions used for dynamic tagging of messages *)
            val mutable tag_generators : (unit -> Handlers.tag) list
              
              
            (** {3 Classic logging Methods}
Each of these methods takes an optional [tag list], then a set of parameters the way a printf function does. If the log level of the instance is low enough, a log item will be created theb passed to the handlers.

Example : 
{[logger#warning "Unexpected value: %s" (to_string my_value)]}
 *)
              
            method flash : 'a. ?tags:string list -> ('a, unit, string, unit) format4 -> 'a
            method error : 'a. ?tags:string list -> ('a, unit, string, unit) format4 -> 'a
            method warning : 'a. ?tags:string list -> ('a, unit, string, unit) format4 -> 'a
            method info : 'a. ?tags:string list -> ('a, unit, string, unit) format4 -> 'a
            method trace : 'a. ?tags:string list -> ('a, unit, string, unit) format4 -> 'a
            method debug : 'a. ?tags:string list -> ('a, unit, string, unit) format4 -> 'a
                 

                 
            (** {3 Lazy logging methods} 
Each of these methods takes a [string lazy_t] as an input (as well as the optional [tag list]. If the log level of the instance is low enough, the lazy value will forced into a [string], a log item will be created then passed to the handlers.

Example:
{[logger#ldebug (lazy (heavy_calculation () ))]}
*)

          
            method ldebug : ?tags:string list -> string lazy_t -> unit
            method ltrace : ?tags:string list -> string lazy_t -> unit
            method linfo : ?tags:string list -> string lazy_t -> unit
            method lwarning : ?tags:string list -> string lazy_t -> unit
            method lerror : ?tags:string list -> string lazy_t -> unit
            method lflash : ?tags:string list -> string lazy_t -> unit

            (** {4 String logging methods} 
Each of these methods takes a [string] as an input (as well as the optional [tag list]. 

Example:
{[logger#sdebug string_variable]}
*)

          
            method sdebug : ?tags:string list -> string -> unit
            method strace : ?tags:string list -> string -> unit
            method sinfo : ?tags:string list -> string -> unit
            method swarning : ?tags:string list -> string -> unit
            method serror : ?tags:string list -> string -> unit
            method sflash : ?tags:string list -> string -> unit

                 
            (** {3 Other methods} *)

                 
            (** Sets the log level of the logger instance. *)    
            method set_level : log_level  -> unit
                 
            (** Adds a handler to the logger instance. *)
            method add_handler : Handlers.t -> unit

            (** Will add a tag to each log message, resulting from the call of the supplied fonction (called each time a message is logged)*)
            method add_tag_generator: (unit -> Handlers.tag) -> unit

            (** Sets the propagate attribute, which decides whether messages passed to this logger are propagated to its ancestors' handlers. *)
            method set_propagate : bool -> unit

            (** {4 Internal methods} *)

            (** Returns the list of handlers of the logger *)
            method get_handlers : Handlers.t list

            (** Returns this logger level if it is not [None], else searches amongst ancestors for the first defined level; returns [NoLevel] if no level can be found. *) 
            method effective_level : log_level

            method set_propagate : bool -> unit

            method add_tag_generator: (unit -> Handlers.tag) -> unit
          end

  (** [make_logger name level handlers_descs] 
      creates a new logger instance from the given arguments,
      then register it internally, and returns it.  *)
  val make_logger :
     ?propagate:bool -> string -> log_level  -> Handlers.desc list -> logger

  (** Returns a registered logger by name. *)
  val get_logger : string -> logger

end
