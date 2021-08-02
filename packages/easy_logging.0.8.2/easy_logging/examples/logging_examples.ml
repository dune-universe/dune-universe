(*
   {3 Preamble}
*)
open Easy_logging

(*
   {2 Presentation}
   {3 Using different log levels}

   A logger has one method for each level. Each of these methods takes the same arguments that the printf function does.
   *)
let logger_1 = Logging.make_logger "_1_ Demo" Debug [Cli Debug];;

logger_1#debug   "This is a %s message" "debug";
logger_1#trace   "This is a %s message" "trace";
logger_1#info    "This is an info message";
logger_1#warning "This is a warning message";
logger_1#error   "This is an error message";
logger_1#flash   "This is a FLASH message";


(*
   {4 Log lazy messages}
For each level, there is also a method, prefixed with «l», that logs messages of type [string Lazy.t]
*)
let heavy_calculation () = "heavy result" in
logger_1#ldebug (lazy (heavy_calculation ()));


(*
   {4 Log string}
You might also want to log a [string],
*)
let my_string = "such a beautiful string" in
logger_1#sdebug my_string;




(*
   {3 Creating a sublogger}
logger_1_sublogger will use logger_1 handlers and level
*)
let logger_1_sublogger = Logging.get_logger "_1_ Demo.sublogger"
in
logger_1_sublogger#set_level Info;


(*
 Use printf like formatting
*)
logger_1_sublogger#warning "This is %s" "format !!";


let h = Handlers.make (Cli Debug) in
h.fmt <- Logging_internals.Formatters.format_color;

logger_1#info "test";
(*
prefix log method with « s » to log static strings
*)
let mystr = "this is a string" in
logger_1_sublogger#sinfo mystr;
(*
 use automatic tags
*)
let counter = ref 0 in
let tag () =
  counter := !counter + 1;
  "M "^ (string_of_int (!counter))
in
logger_1#add_tag_generator tag;
logger_1#info "first message";
logger_1#info "second message";
logger_1#info "third message";

(*
 you can also add tags by hand
*)

logger_1#info ~tags:["OOO"] "another message";
(*
   {2 Modifying the level of a handler}
    *)
let logger_4_root = Logging.get_logger "" in
logger_4_root#error "WTF1";

let h = Handlers.make (Cli Debug) in
let logger_4_sub = Logging.make_logger "_4_ handlerLevelTest" Debug [] in
logger_4_sub#error "WTF2";
logger_4_sub#add_handler h;
logger_4_sub#debug "this message is displayed";
Handlers.set_level h Info;
logger_4_sub#debug "this message is not displayed";
(*
   {2 Modifying the file handler defaults}
    *)


let h_config : Handlers.config =
  {file_handlers = {
      logs_folder= "test/";
      truncate= false;
      file_perms=0o664;
      date_prefix=None;
      versioning=None;
      suffix=".log";
    } } in
Logging.set_handlers_config h_config;;
let logger_5 = Logging.make_logger
    "_4_ File logger demo" Debug [File ("test", Debug)];;
logger_5#info "this is a message";
assert (Sys.file_exists "test/test.log");
(*
   {2 Subloggers}
    *)
let logger_6_A = Logging.get_logger "_6_ SubLoggers.A"
and logger_6_AB = Logging.get_logger "_6_ SubLoggers.A.B"
and logger_6_AC = Logging.get_logger "_6_ SubLoggers.A.C" in
let h = Handlers.make (Cli Debug) in
logger_6_A#add_handler h; logger_6_AC#add_handler h;
logger_6_A#set_level Debug;
logger_6_A#info "one line";
logger_6_AB#info "another line";
logger_6_AC#warning "two lines";
(*
   {2 Json formatter}
    *)
let logger_8 = Logging.get_logger "_8_ Json Formatter"
and h = Handlers.make (Cli Debug) in
logger_8#set_level Debug;
h.fmt <- Logging_internals.Formatters.format_json;
logger_8#add_handler h;
logger_8#info ~tags:["t1"]"it is ok";
logger_8#warning ~tags:["t1"; "t2"] "is it json\"\nis it";
(*
   {2 Filters}
    *)
let logger_9 = Logging.get_logger "_9_ Handler filter" in
logger_9#set_level Debug;
let h = Handlers.make (Cli Debug) in
Handlers.add_filter h (fun _ -> false);
logger_9#warning "this is not printed"

(*
   {2 Rotating File}
*)

let logger_10 = Logging.make_logger
    "_10_rotating" Debug
    [RotatingFile ("rotating", Debug, 2, 3)];;
let c = ref 0 in
for i=0 to 1000 do
  logger_10#info "This is line # %i" !c;
  incr c
done
