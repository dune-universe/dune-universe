type t = string

let pp ppf s = Format.fprintf ppf "%S" s

let equal (x : t) y = x = y

let to_string s = s

let invalid_chars = [ '/'; ':'; '#' ]

let escape_string =
  String.map (fun c -> if List.mem c invalid_chars then '-' else c)

let parse s =
  if List.exists (fun c -> String.contains s c) invalid_chars then
    Rresult.R.error_msg "String contains an invalid character"
  else Ok s

let of_string_exn s = parse s |> Rresult.R.failwith_error_msg
