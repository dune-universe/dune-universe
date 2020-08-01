open Js_of_ocaml
open Js

let compile src =
  let open Unsafe in
  fun_call (js_expr "compile") [| Js.Unsafe.inject src |]
;;

(*
external compile
  :  js_string Js.t
  -> uint32Array Js.t Promise.t Js.t
  = "compile_x"
*)
