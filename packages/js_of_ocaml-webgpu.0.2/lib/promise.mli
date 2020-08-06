open Js_of_ocaml

type 'a t

val _Promise : 'a
val _catch : 'a t Js.t -> ('b -> 'a t Js.t) -> 'a t Js.t

val _then
  :  'a t Js.t
  -> ?catch:('c -> 'b t Js.t)
  -> ('a -> 'b t Js.t)
  -> 'b t Js.t

val resolve_value : 'a -> 'a t Js.t
val resolve_promise : 'a t Js.t -> 'a t Js.t
val reject_value : 'a -> 'a t Js.t
val reject_promise : 'a t Js.t -> 'a t Js.t
val race : 'a t Js.t Js.js_array Js.t -> 'a t Js.t
val all : 'a t Js.t Js.js_array Js.t -> 'a t Js.t
