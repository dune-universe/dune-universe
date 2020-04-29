type header_list = (string * string) list [@@deriving show]

let pp_header fmt header =
  Format.fprintf fmt "@[%s@ %a@]" "Header.of_list" pp_header_list
    (Cohttp.Header.to_list header)
