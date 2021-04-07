let src = Logs.Src.create "mylib" ~doc:"logs for mylib"
module Log = (val (Logs.src_log src : (module Logs.LOG)))
let () =
  Log.debug (fun logger-function -> logger-function "Hello %s!" "World")
