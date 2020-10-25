(**************************************************************************)
(*                                                                        *)
(*                                FADBADml                                *)
(*                                                                        *)
(*           OCaml port by François Bidet and Ismail Bennani              *)
(*     Based on FADBAD++, written by Ole Stauning and Claus Bendtsen      *)
(*                                                                        *)
(*                          Copyright 2019-2020                           *)
(*                                                                        *)
(*   This file is distributed under the terms of the CeCILL-C license.    *)
(*                                                                        *)
(**************************************************************************)

module Op = Fadbad.OpFloat
module F = Fadbad.F(Op)
module BF = Fadbad.B(F)

type diff1 =
  {
    value: F.t;
    dx:    F.t;
    dy:    F.t;
  }

type diff2 =
  {
    value: float;
    dx:    float;
    dy:    float;
    dxdx:  float;
    dxdy:  float;
    dydx:  float;
    dydy:  float;
  }

let func x y =
  let open BF in
  let z = sqrt x in
  (y * z) + (sin z)

let dfunc x y =
  let open BF in
  let x = lift x in
  let y = lift y in
  let f = func x y in
  let () = diff f 0 1 in
  let () = compute f in
  {
    value = value f;
    dx = deriv x 0;
    dy = deriv y 0;
  }

let ddfunc x y =
  let open F in
  let x = lift x in
  let y = lift y in
  let () = diff x 0 2 in
  let () = diff y 1 2 in
  let f = dfunc x y in
  {
    value = get f.value;
    dx = get f.dx;
    dy = get f.dy;
    dxdx = d f.dx 0;
    dxdy = d f.dx 1;
    dydx = d f.dy 0;
    dydy = d f.dy 1;
  }


let () =
  let x = Op.make 1. in
  let y = Op.make 2. in
  let f  = ddfunc x y in
  let () = print_endline ("x = 1") in
  let () = print_endline ("y = 2") in
  let () = print_endline ("f(x,y) = y * (sqrt x) + (sin (sqrt x))") in
  let () = print_newline () in
  let () = print_endline ("f(x,y) = " ^ (string_of_float f.value)) in
  let () = print_endline ("df/dx(x,y) = " ^ (string_of_float f.dx)) in
  let () = print_endline ("df/dy(x,y) = " ^ (string_of_float f.dy)) in
  let () = print_endline ("df/dxdx(x,y) = " ^ (string_of_float f.dxdx)) in
  let () = print_endline ("df/dxdy(x,y) = " ^ (string_of_float f.dxdy)) in
  let () = print_endline ("df/dydx(x,y) = " ^ (string_of_float f.dydx)) in
  let () = print_endline ("df/dydy(x,y) = " ^ (string_of_float f.dydy)) in
  ()
