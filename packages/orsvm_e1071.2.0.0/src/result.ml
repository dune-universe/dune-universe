type filename = string
type error_message = string

type t = Ok of filename
       | Error of error_message
