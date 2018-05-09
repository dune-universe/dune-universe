let map = Core.map
let map2 = Core.map2

let cos x = map cos x
let sin x = map sin x
let tan x = map tan x
let acos x = map acos x
let asin x = map asin x
let atan x = map atan x
let cosh x = map (fun x -> (exp x +. exp (~-.x)) /. 2.) x
let sinh x = map (fun x -> (exp x -. exp (~-.x)) /. 2.) x
let tanh x = map tanh x

let copysign x y = map2 copysign x y

let ldexp x y = map2 (fun x y -> ldexp x (truncate y)) x y

let atanh x = map (fun x -> log ( (1. -. x) /. (1. +. x)) /. 2. ) x

let log x = map log x
let expm1 x = map expm1 x

let ceil x = map ceil x
let floor x = map floor x

let min x y = map2 min x y
let max x y = map2 max x y

let fmod x y = map2 mod_float x y
let sqrt x = map sqrt x
