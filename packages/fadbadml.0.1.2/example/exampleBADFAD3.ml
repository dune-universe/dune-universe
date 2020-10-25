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

type 'a diff1 =
  {
    value: 'a;
    dx:    'a;
    dy:    'a;
  }

type 'a diff2 =
  {
    value: 'a;
    dx:    'a;
    dy:    'a;
    dxdx:  'a;
    dxdy:  'a;
    dydx:  'a;
    dydy:  'a;
  }


module Func (Op : Fadbad.OpS) =
  struct
    let exec x y =
      let open Op in
      let z = sqrt x in
      (y * z) + (sin z)
  end

module DFunc (Op : Fadbad.OpS) =
  struct
    module B = Fadbad.B(Op)
    module Func = Func(B)

    let exec x y =
      let open B in
      let x = lift x in
      let y = lift y in
      let f = Func.exec x y in
      let () = diff f 0 1 in
      let () = compute f in
      {
        value = value f;
        dx = deriv x 0;
        dy = deriv y 0;
      }
  end

module DDFunc (Op : Fadbad.OpS) =
  struct
    module F = Fadbad.F(Op)
    module DFunc = DFunc(F)

    let exec x y =
      let open F in
      let x = lift x in
      let y = lift y in
      let () = diff x 0 2 in
      let () = diff y 1 2 in
      let f = DFunc.exec x y in
      {
        value = get f.value;
        dx = get f.dx;
        dy = get f.dy;
        dxdx = d f.dx 0;
        dxdy = d f.dx 1;
        dydx = d f.dy 0;
        dydy = d f.dy 1;
      }
  end


let () =
  let module DDFunc = DDFunc(Op) in
  let x = Op.make 1. in
  let y = Op.make 2. in
  let f  = DDFunc.exec x y in
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
