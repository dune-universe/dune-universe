

(** Possible level of a log item. *)
type level =
  | Debug
  | Info
  | Warning
  | Error
  | Flash
  | NoLevel

let show_level lvl = match lvl with
  | Debug    -> "Debug"
  | Info     -> "Info"
  | Warning  -> "Warning"
  | Error    -> "Error"
  | Flash    -> "Flash"
  | NoLevel  -> "NoLevel"

       
module type HandlersT =
  sig
    
    (** Type of a handler *) 
    type t
       
    type tag
       
    type log_item = {
        level : level;
        logger_name : string;
        msg : string;
        tags : tag list
      }
    type log_formatter = log_item -> string

    (** Applies the handler to a [log_item] *)
    val apply : t -> log_item -> unit

    (** Type used to instantiate a handler*)
    type desc
    (** Instantiates a handler *)
    val make : desc -> t

  end                   
