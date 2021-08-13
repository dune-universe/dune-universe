type ('error, 'value) t =
  | Error of 'error
  | Value of 'value

let error msg = Error msg
let value value = Value value

let map f result =
  match result with
  | Error error -> Error error
  | Value value -> Value (f value)
