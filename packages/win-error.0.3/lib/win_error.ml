
include Win_error_t

let of_unix_error = function
  | Unix.EUNKNOWNERR code -> Win_error_of_int.of_int (-code)
  | _ -> None

let to_string = Win_error_to_string.to_string

let error_message e =
  if Sys.os_type = "Win32"
  then
    (match of_unix_error e with
    | None -> Unix.error_message e
    | Some x -> to_string x)
  else Unix.error_message e
