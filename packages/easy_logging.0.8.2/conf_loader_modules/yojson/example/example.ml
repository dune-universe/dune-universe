open Easy_logging_yojson
let config = {| 
{loggers: 
    [{"name" : "test_7", "level" : "debug", 
      "handlers": [{"cli" : {"level" :"info"}}]}]
}
|};;
Logging.load_global_config_str config;
let logger = Logging.get_logger "test_7" in
logger#info "this message";
logger#debug "but not this one";

(* we print the tree here because we have the facility to transform yojson to str*)
let tree = Logging.tree_to_yojson () in
let tree_str = Yojson.Safe.to_string tree in

logger#info "This is the tree: %s" tree_str;
