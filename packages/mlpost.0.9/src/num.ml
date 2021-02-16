(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Johannes Kanig, Stephane Lescuyer                       *)
(*  Jean-Christophe Filliatre, Romain Bardou and Francois Bobot           *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

open Types

type t = float

let zero = 0.

let one = 1.

let minus_one = -1.

let two = 2.

let of_float = Misc.id

let num_of_int = float

let bp f = f

let pt f = 0.99626 *. f

let cm f = 28.34645 *. f

let mm f = 2.83464 *. f

let inch f = 72. *. f

let pi = 3.1415926535897932384626433832795029

let pi_div_180 = pi /. 180.0

let deg2rad f = pi_div_180 *. f

let is_zero f = abs_float f < 0.0001

type scale = float -> t

let addn = ( +. )

let subn = ( -. )

let multn = ( *. )

let multf = multn

let divn = ( /. )

let divf x f = divn x f

let maxn = max

let minn = min

let gmean f1 f2 = sqrt ((f1 *. f1) +. (f2 *. f2))

let fold_max f = List.fold_left (fun w p -> maxn w (f p))

let fold_min f = List.fold_left (fun w p -> minn w (f p))

let if_null n n1 n2 = if is_zero n then n1 else n2

module Scale = struct
  let bp x y = bp (x *. y)

  let pt x y = pt (x *. y)

  let cm x y = cm (x *. y)

  let mm x y = mm (x *. y)

  let inch x y = inch (x *. y)
end

module Infix = struct
  let ( +/ ) = addn

  let ( -/ ) = subn

  let ( */ ) = multn

  let ( // ) = divn

  let ( *./ ) = multf

  let ( /./ ) = divf
end

let neg x = 0. -. x

let abs = abs_float

(* TeX units *)
let xlength p =
  let xmin, _, xmax, _ = Gentex.get_dimen_pt (Gentex.create p) in
  xmax -. xmin

let ylength p =
  let _, ymin, _, ymax = Gentex.get_dimen_pt (Gentex.create p) in
  ymax -. ymin

let pic s = mkPicture (mkPITex s)

let em_factor () = xlength "m"

let ex_factor () = ylength "x"

let em f = f *. em_factor ()

let ex f = f *. ex_factor ()
