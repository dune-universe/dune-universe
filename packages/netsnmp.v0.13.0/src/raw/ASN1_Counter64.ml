type t = {
  high : int
; low : int
}

external counter64_to_string : t -> string = "caml_counter64_to_string"
let to_string = counter64_to_string
