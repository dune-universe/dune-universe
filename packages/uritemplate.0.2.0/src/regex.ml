let for_tokens = Str.regexp "{[^{]+}\\|[^{}]+"

let for_is_var_expr = Str.regexp "^{.*}"

let for_prefix = Str.regexp "{\\([\\.#+/\\.;\\?&]?\\)\\([a-zA-Z0-9\\.%,_\\*:]+\\)}"

let for_encode_reserved = Str.regexp "[^A-Za-z0-9-_.~*'()]"

let for_encode_full = Str.regexp "[^A-Za-z0-9;,/\\?:@&=\\+$-_\\.!~\\*'()#]"

let compsite_from_var_name = Str.regexp "\\(.+\\)\\*"

let trim_from_var_name = Str.regexp "\\(.+\\):\\([0-9]+\\)"
