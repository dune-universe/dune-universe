let get = function
  | Some v -> v
  | None -> raise (Invalid_argument "option is None")

let value opt ~default = match opt with Some v -> v | None -> default
