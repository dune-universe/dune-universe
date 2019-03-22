

   
type level =
  | Debug
  | Info
  | Warning
  | Error
  | Flash
[@@deriving show { with_path = false }]

type log_item = {
    level : level;
    logger_name : string;
    msg : string;
  }

type log_formatter = log_item -> string

                
module type HandlersT =
  sig
    
    type t 

    val set_formatter : t -> log_formatter -> unit
    val set_level : t -> level -> unit
    val apply : t -> log_item -> unit
    type desc
    val make : desc -> t
  end                   
