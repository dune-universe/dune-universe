open Phantom_algebra.Core

let v = vec3 1. 2. 3.
let w = vec3 3. 2. 1.
let u = scalar 2. + cross (v + w) (v - w)
let rot = rotation u v 1.
let r = w + rot * v

;; Format.printf "Readme: \x1b[92m[✔]\x1b[97m@."
