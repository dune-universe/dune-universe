(* contains the line and character span of where the error occured *)

type t = {
  line : int
; start_char : int
; end_char : int
; msg : string
}

exception Json_error_info of t

let create_from_lexbuf lexbuf emsg =
  let (eline, schar, echar) = Lexxer_utils.error_pos lexbuf in
  { line = eline; start_char = schar; end_char = echar; msg = emsg }

let to_string info =
  let loc = Printf.sprintf "line %d chars %d-%d" info.line info.start_char info.end_char in
  info.msg ^ " at " ^ loc
