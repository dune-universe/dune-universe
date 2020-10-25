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

module OpString =
  struct
    type scalar = float
    type elt = string
    type t = elt ref

    let create () = ref ""
    let make s = ref s

    let get s = !s
    let ( !! ) = get
    let to_string s = !s
    let string_of_scalar f = string_of_float f
    let string_of_elt e = e

    let copy s = ref !s
    let deepcopy = copy

    let integer i = ref (string_of_int i)
    let zero () = integer 0
    let one () = integer 1
    let two () = integer 2

    let scale s f = ref ("(" ^ (string_of_scalar f) ^ ") * (" ^ !s ^ ")")
    let translate s f = ref ("(" ^ (string_of_scalar f) ^ ") + (" ^ !s ^ ")")

    let ( ~+ ) s = ref ("+(" ^ !s ^ ")")
    let ( ~- ) s = ref ("-(" ^ !s ^ ")")

    let ( + ) s1 s2 =
      if s1 = zero () then
        copy s2
      else if s2 = zero () then
        copy s1
      else
        ref ("(" ^ !s1 ^ ") + (" ^ !s2 ^ ")")

    let ( += ) s1 s2 =
      let () =
        if s1 = zero () then
          s1 := !s2
        else if s2 = zero () then
          ()
        else
          s1 := ("(" ^ !s1 ^ ") + (" ^ !s2 ^ ")")
      in
      s1

    let ( - ) s1 s2 =
      if s1 = zero () then
        -s2
      else if s2 = zero () then
        copy s1
      else
        ref ("(" ^ !s1 ^ ") - (" ^ !s2 ^ ")")


    let ( -= ) s1 s2 =
      let () =
        if s1 = zero () then
          s1 := get (-s2)
      else if s2 = zero () then
          ()
      else
        s1 := ("(" ^ !s1 ^ ") - (" ^ !s2 ^ ")")
      in
      s1


    let ( * ) s1 s2 =
      let zero = zero () in
      if s1 = zero || s2 = zero then
        zero
      else if s1 = one () then
        copy s2
      else if s2 = one () then
        copy s1
      else
        ref ("(" ^ !s1 ^ ") * (" ^ !s2 ^ ")")

    let ( *= ) s1 s2 = print_endline "( *= ) not implemented"; assert false

    let ( / ) s1 s2 =
      if s2 = one () then
        copy s1
      else if s1 = zero () then
        zero ()
      else
        ref ("(" ^ !s1 ^ ") / (" ^ !s2 ^ ")")

    let ( /= ) s1 s2 = print_endline "( /= ) not implemented"; assert false

    let ( ** ) s1 s2 =
      if s1 = one () || s2 = zero () then
        one ()
      else
        ref ("(" ^ !s1 ^ ") ** (" ^ !s2 ^ ")")

    let unop op s = ref (op ^ " (" ^ !s ^ ")")
    let inv = unop "inv"
    let sqr = unop "sqr"
    let sqrt = unop "sqrt"
    let log = unop "log"
    let exp = unop "exp"
    let sin = unop "sin"
    let cos = unop "cos"
    let tan = unop "tan"
    let asin = unop "asin"
    let acos = unop "acos"
    let atan = unop "atan"

    let ( = ) x y = !x = !y
    let ( <> ) x y = assert false
    let ( < ) x y = assert false
    let ( <= ) x y = assert false
    let ( > ) x y = assert false
    let ( >= ) x y = assert false
  end

module F = Fadbad.F(OpString)

module Func (Op : Fadbad.OpS) =
  struct
    let exec x y =
      let open Op in
      let z = sqrt x in
      (y * z) + (sin z)
  end

let fad () =
  let module F = Fadbad.F(OpString) in
  let module Func = Func(F) in
  let x = F.make "x" in
  let y = F.make "y" in
  let () = F.diff x 0 2 in
  let () = F.diff y 1 2 in
  let f = Func.exec x y in
  let f_val = F.get f in
  let dfdx = F.d f 0 in
  let dfdy = F.d f 1 in
  let () = print_endline ("f(x,y) = " ^ f_val) in
  let () = print_endline ("dfdx(x,y) = " ^ dfdx) in
  let () = print_endline ("dfdy(x,y) = " ^ dfdy) in
  ()

let bad () =
  let module B = Fadbad.B(OpString) in
  let module Func = Func(B) in
  let x = B.make "x" in
  let y = B.make "y" in
  let f = Func.exec x y in
  let () = B.diff f 0 1 in
  let () = B.compute f in
  let f_val = B.get f in
  let dfdx = B.d x 0 in
  let dfdy = B.d y 0 in
  let () = print_endline ("f(x,y) = " ^ f_val) in
  let () = print_endline ("dfdx(x,y) = " ^ dfdx) in
  let () = print_endline ("dfdy(x,y) = " ^ dfdy) in
  ()

let () =
  let () = print_endline "FAD:" in
  let () = fad () in
  let () = print_newline () in
  let () = print_endline "BAD:" in
  let () = bad () in
  let () = print_newline () in
  ()
