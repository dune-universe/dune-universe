open! Core_kernel
include Json_object.Utils

let submit_text markup =
  let field =
    match markup with
    | `markdown -> "submit_text"
    | `HTML -> "submit_text_html"
  in
  required_field field string
;;
