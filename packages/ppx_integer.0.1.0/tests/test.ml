(*
~/.opam/4.06.1/lib/ppx_integer/ppx.exe tests/test.ml
*)

module Ppx_integer = struct
  let _u = string_of_int
  let _U = string_of_int
  let _V = string_of_int
end

[%%ppx_integer]

let a = 1234u
let b = 0x1234U

module X = struct
  let c = 1234V
  let d = 0o1234V
end

module Y = struct
  let a' = 1234u
end
