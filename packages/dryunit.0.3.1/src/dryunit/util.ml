let not_implemented feature =
  failwith ("feature `" ^ feature ^ "` is not implemented yet")

let unwrap_or default = function
  | Some v -> v
  | None -> default

let format = Printf.sprintf

let split pattern value =
  Str.split (Str.regexp pattern) value
