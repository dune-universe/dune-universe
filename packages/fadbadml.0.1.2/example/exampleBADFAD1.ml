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

let func x y =
  let open BF in
  let z = sqrt x in
  (y * z) + (sin z)

let () =
  let x = BF.make 1. in
  let y = BF.make 2. in
  let () = F.diff (BF.value x) 0 2 in
  let () = F.diff (BF.value y) 1 2 in
  let f  = func x y in
  let () = BF.diff f 0 1 in
  let () = BF.compute f in
  let f_val = BF.get f in
  let dfdx = BF.d x 0 in
  let dfdy = BF.d y 0 in
  let dfdxdx = F.d (BF.deriv x 0) 0 in
  let dfdxdy = F.d (BF.deriv x 0) 1 in
  let dfdydx = F.d (BF.deriv y 0) 0 in
  let dfdydy = F.d (BF.deriv y 0) 1 in
  let () = print_endline ("x = 1") in
  let () = print_endline ("y = 2") in
  let () = print_endline ("f(x,y) = y * (sqrt x) + (sin (sqrt x))") in
  let () = print_newline () in
  let () = print_endline ("f(x,y) = " ^ (string_of_float f_val)) in
  let () = print_endline ("df/dx(x,y) = " ^ (string_of_float dfdx)) in
  let () = print_endline ("df/dy(x,y) = " ^ (string_of_float dfdy)) in
  let () = print_endline ("df/dxdx(x,y) = " ^ (string_of_float dfdxdx)) in
  let () = print_endline ("df/dxdy(x,y) = " ^ (string_of_float dfdxdy)) in
  let () = print_endline ("df/dydx(x,y) = " ^ (string_of_float dfdydx)) in
  let () = print_endline ("df/dydy(x,y) = " ^ (string_of_float dfdydy)) in
  ()
