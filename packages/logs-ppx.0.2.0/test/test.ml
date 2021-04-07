let src = Logs.Src.create "mylib" ~doc:"logs for mylib"
module Log = (val Logs.src_log src : Logs.LOG)

let () = [%log debug "Hello %s!" "World"]
