open Js_of_ocaml

(* CR-someday: something something unboxed?
   https://bucklescript.github.io/blog/2020/02/07/union-types-in-bucklescript
*)

type ('a, 'b) t

val of_a : 'a Js.t -> ('a Js.t, _) t Js.t
val of_b : 'b Js.t -> (_, 'b Js.t) t Js.t

module Unsafe : sig
  val get_a : ('a Js.t, _) t Js.t -> 'a Js.t
  val get_b : (_, 'b Js.t) t Js.t -> 'b Js.t
end
