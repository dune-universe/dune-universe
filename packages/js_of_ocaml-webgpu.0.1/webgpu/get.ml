open Js_of_ocaml
open Js

let get () = Unsafe.eval_string "window.navigator.gpu"
