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
