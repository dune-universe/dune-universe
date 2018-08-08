open Variant

let _ = M.Foo (* ? M.t *) 1

let _ = if true then (M.Foo 1, []) else assert false


