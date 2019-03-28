Logging module for OCaml.

Quick start :
```ocaml
open Easy_logging
logger = Logging.make_logger "my_logger" Debug [Cli Debug];;
logger#info "log_message";;
```

See documentation at https://sapristi.github.io/easy_logging/easy_logging