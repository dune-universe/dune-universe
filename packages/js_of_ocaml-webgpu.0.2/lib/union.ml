open Js_of_ocaml
open Js.Unsafe

type ('a, 'b) t = any

let of_a = coerce
let of_b = coerce

module Unsafe = struct
  let get_a = coerce
  let get_b = coerce
end
