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
module B = Fadbad.B(Op)

let func x y =
  let open B in
  let z = sqrt x in
  (y * z) + (sin z)

let () =
  let x = B.make 1. in
  let y = B.make 2. in
  let f  = func x y in
  let () = B.diff f 0 1 in
  let () = B.compute f in
  let f_val = B.get f in
  let dfdx = B.d x 0 in
  let dfdy = B.d y 0 in
  let () = print_endline ("x = 1") in
  let () = print_endline ("y = 2") in
  let () = print_endline ("f(x,y) = " ^ (string_of_float f_val)) in
  let () = print_endline ("df/dx(x,y) = " ^ (string_of_float dfdx)) in
  let () = print_endline ("df/dy(x,y) = " ^ (string_of_float dfdy)) in
  ()
