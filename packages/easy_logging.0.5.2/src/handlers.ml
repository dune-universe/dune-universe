(** 
This is the [Handlers] module. It provides simple yet adaptable handlers implementation.

 *)


open Easy_logging_types

(** {1 Type definitions } *)

type tag = string
type log_item = {
    level : Easy_logging_types.level;
    logger_name : string;
    msg : string;
    tags : string list
  }
              
type log_formatter = log_item -> string
type filter= log_item -> bool

(** type of a handler *)
type t =
  {
    mutable fmt : log_formatter;
    mutable level : Easy_logging_types.level;
    mutable filters: filter list;
    output : out_channel;
  }

(**
A handler is made of:
 - a formatter that transforms a log item into a string.
 - a level used to filter out items.
 - an array of possible additional custom filters.
 - an [out_channel], where log strings are outputed by the function [Pervasives.output_string].

 *)
  
  
(** {1 Formatting functions} *)

let reduce (f: 'a -> 'a -> 'a) (l: 'a list) (d: 'a) =
  let rec aux l res =
    match l with
    | [] -> res
    | h::t ->
       let res' = f res h in
       aux t res'
  in
  match l with
  | [] -> d
  | h::t -> aux t h 
  
let format_tags (tags : string list) =
  match tags with
  | [] -> ""
  | _ -> 
     let elems_str = reduce (fun s e -> s ^ " | " ^ e) tags ""
     in "[" ^ elems_str ^ "] "

(** Auxiliary functions. *)

let format_default (item : log_item) =
  Printf.sprintf "%-6.3f %-10s %-20s %s%s" (Sys.time ())
    (show_level item.level)
    item.logger_name
    (format_tags item.tags)
    item.msg
(** Human readable log messages. *)
      
let format_color (item : log_item) =
  
  let level_to_color lvl =
    match lvl with
    | Flash -> Colorize.LMagenta
    | Error -> Colorize.LRed
    | Warning -> Colorize.LYellow
    | Info -> Colorize.LBlue
    | Trace -> Colorize.Cyan
    | Debug -> Colorize.Green
    | NoLevel -> Colorize.Default
  in
  
  let item_level_fmt = Colorize.format [ Fg (level_to_color item.level)]  (show_level item.level)
  and logger_name_fmt = Colorize.format [ Underline] item.logger_name
  and item_msg_fmt =
    match item.level with
    | Flash -> Colorize.format [ Fg Black; Bg LMagenta] item.msg
    | _ -> item.msg in

  Format.pp_set_max_indent Format.str_formatter 200;
  Format.sprintf "@[<hov 2>[%-6.3f %-20s %-30s] %s %s@]"
    (Sys.time ())
    item_level_fmt
    logger_name_fmt
    (format_tags item.tags)
    item_msg_fmt
(** Human readable log messages, with level depending colors.*)
  
let format_json (item: log_item) =
  let format_tags tags =
    match tags with
    | [] -> "[]"
    | _ -> 
       let elems_str = reduce (fun s e ->
                           s^", \""^(String.escaped e)^"\"") tags ""
       in "[" ^ elems_str ^ "] "
                              
  in
  
  Printf.sprintf
    "{\"level\": \"%s\", \"logger_name\": \"%s\", \"message\": \"%s\", \"tags\": %s}" 
    (show_level item.level)
    (String.escaped item.logger_name)
    (String.escaped item.msg)
    (format_tags item.tags)
(** JSON logs for software interoperability. *)

(** {1 Handlers creation helpers } *)
      
let make_cli_handler level =
  {fmt = format_color;
   level = level;
   output = stdout;
   filters = []}


  
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
(** Sets how log files are created when using make_file_handler *)

         
let make_cli_handler level =
  {fmt = format_color;
   level = level;
   output = stdout;
   filters = []}        
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
   filters = [];
  }
  
  
type desc = | Cli of level | File of string * level
   
let make d = match d with
  | Cli lvl -> make_cli_handler lvl
  | File (f, lvl) -> make_file_handler lvl f
(** Used for quick handler creation, e.g.


 - Cli handler: outputs colored messages to stdout 
   {[ let h = Handlers.make (Cli Debug) ]}
 - File handler : outputs messages to a given file
   {[ let h = Handlers.make (File ("filename", Debug)) ]}
 *)


                   
(** {1 Handlers setup } *)
                   

(** Sets the level of a handler. *)
let set_level (h:t) lvl =
  h.level <- lvl

(** Sets the formatter of a handler. *)
let set_formatter h fmt =
  h.fmt <- fmt

(** Adds a filter to a handler. *)
let add_filter h filter =
  h.filters <- filter::h.filters


(** Auxiliary function.*)
let apply (h : t) (item: log_item) =
  
  if item.level >= h.level && (reduce (&&) (List.map (fun f -> f item) h.filters) true)
  then
    (
      output_string h.output (Printf.sprintf "%s\n" (h.fmt item));
      flush h.output;
    )
