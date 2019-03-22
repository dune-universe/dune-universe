open Easy_logging

(* ************* *)
(* Basic example *)
let logger = Logging.make_logger
               "test" (Some Debug)
               [Cli Debug];;
               
logger#debug "This is a debug message";
logger#info "This is an info message";
logger#warning "This is a warning message";
logger#error "This is an error message";
logger#flash "This is a FLASH message";

(* another logger *)
let sublogger = Logging.make_logger
                  "test.sub" (Some Info)
                  [Cli Debug]
in

(* ***************** *)
(* log lazy messages *)
let heavy_calculation () = "heavy result" in
sublogger#ldebug (lazy (heavy_calculation ()));



(* ********************************** *)
(* Globally modifying logger level :
 * sets the level of all loggers whose
 * name begins with "test"            *)
Logging.set_level "test" (Some Warning);


(* ***************************** *)
(* Custom handlers example:    
 *   logs are stored in a given list ref *)
module MyHandlers =
  struct
    type t = string -> unit
    type desc = string list ref
    let set_formatter _ _ = ()
    let set_level _ _ = ()
    let apply h (item : log_item) = h item.msg
    let make (_internal : desc) =

      fun s -> _internal := s::!_internal
  end

module MyLogging = MakeLogging(MyHandlers)

let l = ref [];;
let mylogger = MyLogging.make_logger "mylogger" (Some Debug) [l];;
mylogger#info "this is a message";
assert (!l = ["this is a message"]);

mylogger#set_level (Some Warning);
mylogger#debug "this message will not be passed to the handler";
assert (!l = ["this is a message"]);
