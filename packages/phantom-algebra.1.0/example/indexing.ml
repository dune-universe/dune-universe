open Phantom_algebra.Core

let v = vec2 0. 1.
let v4 = (vec4 0. 1. 2. 3.)
let pi = 4. *. atan 1.
let r = rotation (vec3 1. 0. 0.) (vec3 0. 1. 0.) (pi /. 6.)
let eye2 = eye d2
let sym = mat2 (vec2 0. 1.) (vec2 1. 0.)
open Tools
#if OCAML_MAJOR>=4 && OCAML_MINOR>=6
let f = "v_x" :=  v.%[x'] =? scalar 0.
let ryy = "r_yy" := r.%[yy'] =? scalar (sqrt 3. /. 2.)
let rx = "r_x" := r.%[x'] =? vec3 (sqrt 3. /. 2.) ~-.0.5 0.

let sel = w'&z'&y'&x'
let sw = "v4_3210" := v4 .%[sel] =? vec4 3. 2. 1. 0.

let msw = "Id[1,0] = xy-sym" := eye2.%[y'&x'] =? sym

let diag = "Id(xx,xy,yy)" := eye2.%[xx'&xy'&yy'] =? vec3 1. 0. 1.
#endif
