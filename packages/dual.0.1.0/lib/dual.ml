(*
 The MIT License                                                                                                                                 
                                                                                                                                                 
 Copyright (c) 2020 Jason D. Nielsen <drjdnielsen@gmail.com>
 *)

(* Dual numbers *)

type t = { re: float; dre: float }

let dual ?y:(y=1.0) x =
  { re = x; dre = y }

let print_dual x =
  Printf.printf "Dual: (%f, %f)\n" x.re x.dre

let equal x y =
  let eps = sqrt Float.epsilon in
  let v1 = (Float.abs (x.re -. y.re)) < eps in
  let v2 = (Float.abs (x.dre -. y.dre)) < eps in
  v1 && v2

let add x y = { re = x.re +. y.re; dre = x.dre +. y.dre }

let sub x y = { re = x.re -. y.re; dre = x.dre -. y.dre }

let neg x =  { re = -. x.re; dre = -. x.dre }

let mul x y = { re = x.re *. y.re; dre = x.dre *. y.re +. x.re *. y.dre }

let div x y =
  { re = x.re /. y.re; dre = (x.dre *. y.re -. x.re *. y.dre) /. (y.re *. y.re) }

let pow x y =
  { re = Float.pow x.re y; dre = x.dre *. y *. (Float.pow x.re  (y -. 1.0)) }

let sqrt x = pow x 0.5

let exp x =
  let ex = Float.exp x.re in
  { re = ex; dre = x.dre *. ex }

let log x = { re = Float.log x.re; dre = x.dre /. x.re }

let sin x = { re = Float.sin x.re; dre = x.dre *. Float.cos(x.re) }

let cos x = { re = Float.cos x.re; dre = -. x.dre *. Float.sin(x.re) }

let tan x = div (sin x) (cos x)

let asin x = { re = Float.asin x.re; dre = x.dre /. (Float.sqrt (1.0 -. x.re *. x.re)) }

let acos x = { re = Float.acos x.re; dre = -. x.dre /. (Float.sqrt (1.0 -. x.re *. x.re)) }

let atan x = { re = Float.atan x.re; dre = x.dre /. (1.0 +. x.re *. x.re) }

let sinh x = { re = Float.sinh x.re; dre = x.dre *. (Float.cosh x.re) }

let cosh x = { re = Float.cosh x.re; dre = x.dre *. (Float.sinh x.re) }

let tanh x =
  let tanhx = Float.tanh x.re in
  { re = tanhx; dre = x.dre *. (1.0 -. (Float.pow tanhx 2.0)) }

let root f x0 =
  let e = Float.sqrt Float.epsilon in
  let rec loop xn =
    let fd = f xn in
    let xnp1 = { re = xn.re -. fd.re /. fd.dre; dre = xn.dre } in
    let error = Float.abs fd.re in
    if error < e then
      xnp1
    else
      loop xnp1
  in
  let out = loop x0 in
  out
