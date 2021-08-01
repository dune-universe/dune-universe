let src = Logs.Src.create "oracle" ~doc:"logs oracle's events"

module Log = (val Logs.src_log src : Logs.LOG)

include Log
