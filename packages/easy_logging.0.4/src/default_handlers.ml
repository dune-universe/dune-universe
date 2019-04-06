(** 
In the [DefaultHandlers] module, handlers have level of their own. Their are two kinds of logger : 

 - Cli handler: outputs colored messages to stdout 
   {[ let h = Default_handlers.make (Cli Debug) ]}
 - File handler : outputs messages to a given file
   {[ let h = Default_handlers.make (File ("filename", Debug)) ]}

 *)


open Easy_logging_types

(** {1 Type definitions } *)
   
(** we don't use tags here *)
type tag = unit

             
type log_item = {
    level : Easy_logging_types.level;
    logger_name : string;
    msg : string;
    tags : tag list
  }
              
type log_formatter = log_item -> string


(** type of a handler *)
type t =
  {
    mutable fmt : log_formatter;
    mutable level : Easy_logging_types.level;
    output : out_channel;
  }

(** {1 Formatting functions} *)

  
let format_default (item : log_item) =
  Printf.sprintf "%-6.3f %-10s %-20s %s" (Sys.time ())
    (show_level item.level)
    item.logger_name
    item.msg
  
      
let format_color (item : log_item) =
  
  let level_to_color lvl =
    match lvl with
    | Flash -> Colorize.LMagenta
    | Error -> Colorize.LRed
    | Warning -> Colorize.LYellow
    | Info -> Colorize.LBlue
    | Debug -> Colorize.Green
    | NoLevel -> Colorize.Default
  in
  
  let item_level_fmt = Colorize.format [ Fg (level_to_color item.level)]  (show_level item.level)
  and logger_name_fmt = Colorize.format [ Underline] item.logger_name
  and item_msg_fmt =
    match item.level with
    | Flash -> Colorize.format [ Fg Black; Bg LMagenta] item.msg
    | _ -> item.msg in
  
  (Printf.sprintf "%-6.3f %-20s %-30s %s" (Sys.time ())
     item_level_fmt
     logger_name_fmt
     item_msg_fmt)

(** {1 Handlers creation and setup utility functions } *)
  
  
let make_cli_handler level =
  {fmt = format_color;
   level = level;
   output = stdout}


  
type file_handlers_config = {
    logs_folder: string;
    truncate: bool;
    file_perms: int}

let file_handlers_defaults = {
    logs_folder = "logs/";
    truncate = true;
    file_perms = 0o660;
  }

type config =
  {mutable file_handlers: file_handlers_config}
let config = {file_handlers = file_handlers_defaults}

let set_config c = config.file_handlers <- c.file_handlers
let make_file_handler level filename  =
  
  if not (Sys.file_exists config.file_handlers.logs_folder)
  then  
    Unix.mkdir config.file_handlers.logs_folder 0o775;

  let open_flags =
    if config.file_handlers.truncate
    then [Open_wronly; Open_creat;Open_trunc]
    else [Open_wronly; Open_creat]
  in
  let oc = 
    open_out_gen open_flags
      config.file_handlers.file_perms
      (config.file_handlers.logs_folder^filename)
      
  in
  {fmt = format_default;
   level = level;
   output = oc;
  }
  
  
type desc = | Cli of level | File of string * level
   
let make d = match d with
  | Cli lvl -> make_cli_handler lvl
  | File (f, lvl) -> make_file_handler lvl f
                  
(** {1 Handlers usage } *)
                   
let set_level (h:t) lvl =
  h.level <- lvl
let set_formatter h fmt =
  h.fmt <- fmt


let apply (h : t) (item: log_item) =
  if item.level >= h.level
  then
    (
      output_string h.output (Printf.sprintf "%s\n" (h.fmt item));
      flush h.output;
    )
