Logging module for OCaml.

Quick start :
```ocaml
open Easy_logging
logger = Logging.make_logger "my_logger" Debug [Cli Debug];;
logger#info "log_message";;
```

See documentation at https://sapristi.github.io/easy_logging/easy_logging


## Changelog

### Version 0.4

 * printf style logging is now the default
 * simplifed configuration in case of multiple loggers (and closer to the python module) :
   the loggers form a tree (based on their name, dots indicating descendence)
    - log items are passed to the handlers of a logger’s ancestors (so that few handlers need initialisation) (possible to override)
    - loggers inherit the level of their ancestor if not set explicitely
 * an additional package easy_logging_yojson provides initialisation of loggers from the json format (with ppx_deriving_yojson), so that configuration can be fully done outside the code.

### Version 0.5

 * Renamed the Default_handlers module to handlers
 * tag type in Handlers is now string (was unit)
 * added the possibility to add filters to handlers
 * added tag_generator feature to loggers, to automatically add tags to all messages passed to a logger

#### version 0.5.1

 * log_level type is direcly accessible from Easy_logging and Easy_logging_yojson modules
 
#### version 0.5.2

 - more file_handler options (timestamp, versioning)
 
### Version 0.6

 - Added more options to file handlers (automatic timestamps and/or versioning of file names)
 - Added CliErr handler to output to stderr
 - modified handlers type so that fully custom handlers can be instantiated
 - cleaned module API
 - log items contains a timestamp

#### Version 0.6.2

 - Added RotatingFile handlers : log file rotation based on file size
