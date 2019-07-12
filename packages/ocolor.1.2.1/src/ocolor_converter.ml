open Ocolor_types

(* Upgrade *)
let color8_of_color4 (c: color4) : color8 =
  Standard c

let color24_of_color4 (c: color4) : color24 =
  let r, g, b = Ocolor_color_space.rgb_of_color4 c in
  { r24 = r; g24 = g; b24 = b }

let color24_of_color8 (c: color8) : color24 =
  match c with
  | Standard c -> color24_of_color4 c
  | Cube6 {r6; g6; b6} ->
    if r6 < 0 || r6 > 5 then raise (Invalid_argument "Red parameter of a color8 should be in [0; 5].");
    if g6 < 0 || g6 > 5 then raise (Invalid_argument "Green parameter of a color8 should be in [0; 5].");
    if b6 < 0 || b6 > 5 then raise (Invalid_argument "Blue parameter of a color8 should be in [0; 5].");
    {r24 = 51 * r6; g24 = 51 * g6; b24 = 51 * b6}
  | Grayscale n ->
    if n < 0 || n > 23 then raise (Invalid_argument "Grayscale parameter of a color8 should be in [0; 23].");
    let l = n * 255 / 23 in { r24 = l; g24 = l; b24 = l }


(* Downgrade *)

let color4_of_color24 ({r24; g24; b24}: color24) : color4 =
  if r24 < 0 || r24 > 255 then raise (Invalid_argument "Red parameter of a color8 should be in [0; 255].");
  if g24 < 0 || g24 > 255 then raise (Invalid_argument "Green parameter of a color8 should be in [0; 255].");
  if b24 < 0 || b24 > 255 then raise (Invalid_argument "Blue parameter of a color8 should be in [0; 255].");
  let c = Ocolor_color_space.closest_color4 (r24, g24, b24) in
  c

let color4_of_color8 (c: color8) : color4 =
  match c with
  | Standard c -> c
  | Cube6 _
  | Grayscale _ -> c |> color24_of_color8 |> color4_of_color24

let color8_of_color24 ({r24; g24; b24}: color24) : color8 =
  if r24 < 0 || r24 > 255 then raise (Invalid_argument "Red parameter of a color8 should be in [0; 255].");
  if g24 < 0 || g24 > 255 then raise (Invalid_argument "Green parameter of a color8 should be in [0; 255].");
  if b24 < 0 || b24 > 255 then raise (Invalid_argument "Blue parameter of a color8 should be in [0; 255].");
  let c = Ocolor_color_space.closest_color8 (r24, g24, b24) in
  c
