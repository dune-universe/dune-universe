external success_code : unit -> int = "get_success_code"

external failure_code : unit -> int = "get_failure_code"

let success_code = success_code ()

let failure_code = failure_code ()

let success () = exit success_code

let failure () = exit failure_code

let is_posix = success_code = 0 && failure_code = 1
