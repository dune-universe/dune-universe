type t =
  | Comment of string
  | Map of { a : int; b : int; name : string; }

let is_comment = function
  | Comment _ -> true
  | Map _ -> false

let pp ppf = function
  | Comment comment -> Format.fprintf ppf "%s" comment
  | Map { a; b; name; } ->
    Format.fprintf ppf "{@[<hov>a = 0x%02x;@ \
                                b = 0x%04x;@ \
                                name = %s;@]}"
      a b name
