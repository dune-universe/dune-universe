open Gamestate

let pp_elt fmtr cell =
  match cell with
  | P1 -> Format.fprintf fmtr "o"
  | P2 -> Format.fprintf fmtr "x"

let pp = Sparse_matrix.pp pp_elt
