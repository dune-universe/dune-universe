(** Is this doc *)

open Easy_logging

(* ************* *)
(* Basic example *)
let logger = Logging.make_logger
               "test_A" Debug
               [Cli Debug];;
               
logger#debug "This is a debug message";
logger#info "This is an info message";
logger#warning "This is a warning message";
logger#error "This is an error message";
logger#flash "This is a FLASH message";

(* another logger *)
let sublogger = Logging.get_logger "test_1.sub"
in
sublogger#set_level Info;

(* ***************** *)
(* log lazy messages *)
let heavy_calculation () = "heavy result" in
sublogger#ldebug (lazy (heavy_calculation ()));

(* *** *)
sublogger#warning "This is %s" "format !!";



(* ********************************** *)
(* Globally modifying logger level :
 * sets the level of all loggers whose
 * name begins with "test"            *)


(* ***************************** *)
(* Custom handlers example:    
 *   logs are stored in a given list ref *)
module MyHandlers =
  struct
    type t = string -> unit
    type tag = unit
    type log_item = {
        level : Easy_logging__.Easy_logging_types.level;
        logger_name : string;
        msg : string;
        tags : tag list
      }
                  
    type log_formatter = log_item -> string

    type desc = string list ref
                  [@@deriving yojson]

    let apply h (item : log_item) = h item.msg
    let make (_internal : desc) =
      fun s -> _internal := s::!_internal

    type config = unit
                [@@deriving of_yojson]
    let default_config = ()
    let set_config = fun _ -> ()
  end

module MyLogging = MakeLogging(MyHandlers)

let l = ref [];;
let mylogger = MyLogging.make_logger "test_2_custom_handlers" Debug [l];;
mylogger#info "this is a message";
assert (!l = ["this is a message"]);

mylogger#set_level Warning;
mylogger#debug "this message will not be passed to the handler";
assert (!l = ["this is a message"]);



(* ************************** *)
(* Tags handlers example:     *)
module TaggedHandlers =
  struct
    type tag =
      | Time
      | Value of int

    type log_item = {
        level : Easy_logging__.Easy_logging_types.level;
        logger_name : string;
        msg : string;
        tags : tag list
      }
    type t = log_item -> unit
                  
    type log_formatter = log_item -> string

    type desc = unit
                  [@@deriving yojson]

    let apply h (item : log_item) = h item

    let rec tags_to_string tags =
      let open Unix in
      match tags with
        
      | Time :: tags' -> 
         let {tm_sec; tm_min; tm_hour;
   	      tm_mday; tm_mon;  _; } :tm
           = time () |> gmtime
         in
         let s = 
           Printf.sprintf "%d/%d %d:%d:%d"
             (tm_mday+1) (tm_mon+1) tm_hour tm_min tm_sec
         in
         s :: tags_to_string tags'
          
      | Value n :: tags' ->
         ( "[Val: "^(string_of_int n)^"]" ) :: tags_to_string tags'

      | [] -> []
         
    let make () =
      fun item ->
      let tags_s = List.fold_left (fun a b -> a ^ " " ^ b) "" (tags_to_string item.tags) in
      tags_s ^ " " ^ item.msg
      |> print_endline

      
    type config = unit
                [@@deriving of_yojson]
    let default_config = ()
    let set_config = fun _ -> ()
  end



module TagsLogging = MakeLogging(TaggedHandlers);;

let tagged_logger = TagsLogging.make_logger "test_3_tagged" Debug [()];;
tagged_logger#info ~tags:[Time; Value 4] "log message with tags";



(* ******************************** *)
(* modifying the level of a handler *)
let rootlogger = Logging.get_logger "" in
rootlogger#error "WTF1";

let h = Default_handlers.make (Cli Debug) in
let logger = Logging.make_logger "test_3_handlerLevelTest" Debug [] in
logger#error "WTF2";
logger#add_handler h;
logger#debug "this message is displayed";
Default_handlers.set_level h Info;
logger#debug "this message is not displayed";


(* *********************************** *)
(* modifying the file handler defaults *)

module H = Default_handlers
let config : H.config =
  {file_handlers = {
     logs_folder= "test/";
     truncate= false;
     file_perms=0o664;}};;
H.set_config config;;
module TestLogging = MakeLogging(H)
let logger = TestLogging.make_logger
               "test_4_file_defaults" Debug [File ("test", Debug)];;
logger#info "this is a message";
assert (Sys.file_exists "test/test");




let lA = Logging.get_logger "test_5.A"
and lAB = Logging.get_logger "test_5.A.B"
and lAC = Logging.get_logger "test_5.A.C" in
let h = Default_handlers.make (Cli Debug) in
lA#add_handler h; lAC#add_handler h;
lA#set_level Debug; 
lA#info "one line";
lAB#info "another line";
lAC#warning "two lines";




let llla = Logging.get_logger "test_6.la" in
let la = Logging.make_logger "test_6" Debug [Cli Debug] in
llla#debug "is this ok?";
la#info "you bet it is!"

