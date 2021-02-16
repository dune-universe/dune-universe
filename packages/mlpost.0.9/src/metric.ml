type t = Tfm.t

open Tfm

(* compute index of the character *)
let to_abs_idx t i = i - t.file_hdr.bc

(* get info struct of character *)
let get_info t i = t.body.char_info.(to_abs_idx t i)

let char_width t c = t.body.width.((get_info t c).width_index)

let char_height t c = t.body.height.((get_info t c).height_index)

let char_depth t c = t.body.depth.((get_info t c).depth_index)

let char_italic t c = t.body.italic.((get_info t c).italic_index)

let char_dims t c =
  let i = get_info t c in
  let b = t.body in
  (b.width.(i.width_index), b.height.(i.height_index), b.depth.(i.depth_index))

let slant t = t.body.param.(0)

let space t = t.body.param.(1)

let space_stretch t = t.body.param.(2)

let space_shrink t = t.body.param.(3)

let x_height t = t.body.param.(4)

let quad t = t.body.param.(5)

let extra_space t = t.body.param.(6)

(* is the size of one em in the font. *)
