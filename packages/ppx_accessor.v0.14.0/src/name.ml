open! Base
open! Import

type t = string

let of_string = Fn.id
let to_constructor_string = Fn.id

let to_lowercase_string t =
  let string = String.lowercase t in
  if Caml.Hashtbl.mem Lexer.keyword_table string then string ^ "_" else string
;;
