open Js_of_ocaml
open Js
open Typed_array
open Js_of_ocaml_webidl.Runtime

val compile : js_string Js.t -> uint32Array Js.t Promise.t Js.t
