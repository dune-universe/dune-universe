open Phantom_algebra
let fn v w =
  (cross v w) + scalar 1.
  let t = fn (vec2 0. 1.) (vec2 1. 0.)
