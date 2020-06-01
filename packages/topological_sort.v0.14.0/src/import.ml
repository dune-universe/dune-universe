include Base
include Stdio

let error_s = Or_error.error_s
let eprint_s sexp = eprintf "%s\n" (sexp |> Sexp.to_string_hum)
