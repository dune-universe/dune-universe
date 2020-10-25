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
module FF = Fadbad.F(F)

let func x y =
  let open FF in
  let z = sqrt x in
  (y * z) + (sin z)

let () =
  let x = F.make 1. in
  let () = F.diff x 0 1 in
  let x = FF.lift x in
  let () = FF.diff x 0 2 in
  let y = FF.make 2. in
  let () = FF.diff y 1 2 in
  (* let () = F.diff y 1 2 in *)
  let f  = func x y in
  let f_val = FF.get f in
  let dfdx = FF.d f 0 in
  let dfdxdx = F.d (FF.deriv f 0) 0 in
  let dfdydx = F.d (FF.deriv f 1) 0 in
  let () = print_endline ("x = 1") in
  let () = print_endline ("y = 2") in
  let () = print_endline ("f(x,y) = " ^ (string_of_float f_val)) in
  let () = print_endline ("df/dx(x,y) = " ^ (string_of_float dfdx)) in
  let () = print_endline ("df/dxdx(x,y) = " ^ (string_of_float dfdxdx)) in
  let () = print_endline ("df/dydx(x,y) = " ^ (string_of_float dfdydx)) in
  ()
