(**
   This is the [Handlers] module. It provides simple yet adaptable handlers implementation.

*)


open Logging_types
open Formatters
(** {1 Type definitions } *)
type log_formatter = log_item -> string
type filter= log_item -> bool

(** Type of a handler *)
type t =
  {
    mutable fmt : log_formatter;
    mutable level : level;
    mutable filters: filter list;
    output : string -> unit;
  }

(**
   A handler is made of:
   - a formatter that transforms a log item into a string.
   - a level used to filter out items.
   - an array of possible additional custom filters.
   - an [output] function, that takes a string and does the output job.

*)


(** {1 Handlers creation helpers } *)

(** Module to create handlers that [output] to stdout or [stderr]. *)
module CliHandler = struct
  let make oc level =
    {fmt = format_color;
     level = level;
     output = (fun s -> output_string oc s; flush oc);
     filters = []}
end

(** Module to create handlers that output to a file. *)
module FileHandler = struct

  (** Configuration when creating a file handler.
      - The [date_prefix] value is an optional Calender Printer String (as specified in the  {{:http://calendar.forge.ocamlcore.org/doc/Printer.html} Calendar documentation }), that will format the time at which the handler is created, and used as a prefix.
      - If the [versioning] value is not None, a versionning number is appended to the filename, based on already existing files. The integer gives the padding of versionning numbers.  THIS OPTION MIGHT DISAPPEAR IN FUTURE RELEASES.
  *)
  type config = {
    logs_folder: string; (** Path in which logs files will be written. *)
    truncate: bool;  (** Truncate/append if file already exists. *)
    file_perms: int;  (** Unix file permissions. *)
    date_prefix : string option; (** Optional date prefix format string. *)
    versioning: int option; (**  Optionaly generate new file name. *)
    suffix: string; (** Constant suffix. *)
  }

  let default_config = {
    logs_folder = "logs/";
    truncate = true;
    file_perms = 0o660;
    date_prefix = Some "%Y%m%d_";
    versioning = None;
    suffix : string = ".log"
  }


  let generate_prefix config  =
    match config.date_prefix with
    | None -> ""
    | Some f ->
      let open CalendarLib in
      let initial_tz = Time_Zone.current () in
      Time_Zone.change Local;
      let now = Calendar.now () in
      let prefix = (Printer.Calendar.sprint f now) in
      Time_Zone.change initial_tz;
      prefix

  let generate_filename config base =
    let rec find_versioned pattern i =
      let name = Printf.sprintf pattern i in
      if Sys.file_exists name
      then find_versioned pattern (i+1)
      else name
    in
    let prefix = generate_prefix config
    and suffix = config.suffix
    and folder = config.logs_folder
    in
    match config.versioning
    with
    | None -> Filename.concat folder prefix^base^suffix
    | Some i ->
      let filename_pattern_string =
        Filename.concat folder
          (Printf.sprintf "%s%s_%%0%ii%s" prefix base i suffix) in
      let filename_pattern  = Scanf.format_from_string filename_pattern_string "%i" in
      find_versioned filename_pattern 0

  let create_file config filename =
    if not (Sys.file_exists config.logs_folder)
    then
      Unix.mkdir config.logs_folder 0o775;

    let open_flags =
      if config.truncate
      then [Open_wronly; Open_creat;Open_trunc]
      else [Open_wronly; Open_creat]
    in
    open_out_gen open_flags
      config.file_perms filename

(** [make ~config level filename_base] creates a Handler that will output logs to the file created from [filename_base] and [config]
  *)
  let make ?config:(config=default_config) level filename_base =
    let filename = generate_filename config filename_base in 
    let oc = create_file config filename
    in
    {fmt = format_default;
     level = level;
     output = (fun s -> output_string oc s; flush oc);
     filters = [];
    }

(** [make_rotating ~config level filename_base max_kbytes backup_count] creates a Handler that will output logs to a rotating set of files. The files are from [filename_base] and [config]. When the size of a log file is about to exceed [max_kbytes] kb, a new file is created, and older log files are renamed. See {{:https://docs.python.org/3/library/logging.handlers.html#logging.handlers.RotatingFileHandler} the python RotatingFileHandler}.
  *)
  let make_rotating ?config:(config=default_config) level filename_base
      max_kbytes backup_count =

    let bytes_remaining = ref @@ max_kbytes * 1024
    and filename = generate_filename config filename_base in

    let make_rotating_filename n =
      if n = 0
      then filename
      else filename^"."^(string_of_int n)
    and oc = ref (create_file config filename)
    and files_nb =  ref 1 in

    let output = fun s ->
      if !bytes_remaining - String.length s < 0
      then
        begin
          close_out !oc;
          if !files_nb = backup_count + 1
          then
            begin
              Sys.remove (make_rotating_filename backup_count);
              decr files_nb
            end;
          for i = !files_nb downto 1 do
            Sys.rename (make_rotating_filename (i-1)) (make_rotating_filename i)
          done;
          oc := create_file config filename;
          incr files_nb;
          bytes_remaining := max_kbytes * 1024
        end;
      output_string !oc s; flush !oc;
      bytes_remaining := !bytes_remaining - String.length s
    in
    {fmt = format_default;
     level ;
     output ;
     filters = [];
    }


end

type config =
  {file_handlers: FileHandler.config }
let default_config = {file_handlers = FileHandler.default_config}

type desc = | Cli of level | CliErr of level
            | File of string * level | RotatingFile of string * level * int * int

let make ?config:(config=default_config) desc = match desc with
  | Cli lvl -> CliHandler.make stdout lvl
  | CliErr lvl -> CliHandler.make stderr lvl
  | File (f, lvl) -> FileHandler.make ~config:config.file_handlers lvl f
  | RotatingFile (f, lvl, max_bytes, backup_count) ->
    FileHandler.make_rotating ~config:config.file_handlers lvl f max_bytes backup_count

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
      h.output (Printf.sprintf "%s\n" (h.fmt item));
    )
