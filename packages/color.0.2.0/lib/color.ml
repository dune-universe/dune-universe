let round x = int_of_float @@ Gg.Float.round x

let positive_float x y = mod_float (mod_float x y +. y) y

type t = Gg.color

module Hsla = struct
  type t = {h: float; s: float; l: float; a: float}
end

module Rgba' = struct
  type t = {r: float; g: float; b: float; a: float}
end

module Rgba = struct
  type t = {r: int; g: int; b: int; a: float}
end

let of_rgba r g b a = Gg.Color.v_srgbi ~a r g b |> Gg.Color.clamp

let of_rgb red green blue = of_rgba red green blue 1.

let of_rgba' r g b a = Gg.Color.v_srgb ~a r g b |> Gg.Color.clamp

let of_rgb' r g b = of_rgba' r g b 1.

let of_hsla h s l a =
  let clip_hue x = if 360.0 = x then x else positive_float x 360.0 in
  let norm_hue = clip_hue h /. 60. in
  let chr = (1. -. abs_float ((2. *. l) -. 1.)) *. s in
  let m = l -. (chr /. 2.) in
  let x = chr *. (1. -. abs_float (mod_float norm_hue 2. -. 1.)) in
  let make r g b =
    Gg.Color.v_srgb ~a (r +. m) (g +. m) (b +. m) |> Gg.Color.clamp
  in
  if norm_hue < 0. then make 0. 0. 0.
  else if norm_hue < 1. then make chr x 0.
  else if norm_hue < 2. then make x chr 0.
  else if norm_hue < 3. then make 0. chr x
  else if norm_hue < 4. then make 0. x chr
  else if norm_hue < 5. then make x 0. chr
  else if norm_hue < 6. then make chr 0. x
  else make 0. 0. 0.

let of_hsl h s l = of_hsla h s l 1.

let of_hexstring s =
  if String.length s = 4 || String.length s = 7 then
    let short = String.length s = 4 in
    let r' = if short then String.sub s 1 1 else String.sub s 1 2 in
    let g' = if short then String.sub s 2 1 else String.sub s 3 2 in
    let b' = if short then String.sub s 3 1 else String.sub s 5 2 in
    let r = int_of_string_opt ("0x" ^ r') in
    let g = int_of_string_opt ("0x" ^ g') in
    let b = int_of_string_opt ("0x" ^ b') in
    match (r, g, b) with
    | Some r, Some g, Some b ->
        if short then
          Some (of_rgb ((16 * r) + r) ((16 * g) + g) ((16 * b) + b))
        else Some (of_rgb r g b)
    | _ -> None
  else None

let to_rgba' t =
  let color = Gg.Color.to_srgb t in
  { Rgba'.r= Gg.Color.r color
  ; g= Gg.Color.g color
  ; b= Gg.Color.b color
  ; a= Gg.Color.a color }

let to_rgba color =
  let c = to_rgba' color in
  let r = round (255. *. c.r) in
  let g = round (255. *. c.g) in
  let b = round (255. *. c.b) in
  {Rgba.r; g; b; a= c.a}

let to_hsla t =
  let rgba = to_rgba t in
  let red, green, blue, alpha = (rgba.r, rgba.g, rgba.b, rgba.a) in
  let r = float_of_int red /. 255. in
  let g = float_of_int green /. 255. in
  let b = float_of_int blue /. 255. in
  let c_max = max (max red green) blue in
  let c_min = min (min red green) blue in
  let c = c_max - c_min in
  let c' = float_of_int c /. 255. in
  let hue' c =
    if c = 0 then 0.
    else if c_max = red then positive_float ((g -. b) /. c') 6.
    else if c_max = green then ((b -. r) /. c') +. 2.
    else ((r -. g) /. c') +. 4.
  in
  let hue = 60. *. hue' c in
  let lightness = float_of_int (c_max + c_min) /. (255. *. 2.) in
  let saturation =
    if c = 0 then 0. else c' /. (1. -. abs_float ((2. *. lightness) -. 1.))
  in
  {Hsla.h= hue; s= saturation; l= lightness; a= alpha}

let to_hexstring color =
  let c = to_rgba color in
  let to_hex n =
    let repr = Printf.sprintf "%x" n in
    if String.length repr = 1 then "0" ^ repr else repr
  in
  "#" ^ to_hex c.r ^ to_hex c.g ^ to_hex c.b

let to_css_hsla t =
  let {Hsla.h; s; l; a} = to_hsla t in
  if a = 1. then
    Printf.sprintf "hsl(%.2f, %.2f%s, %.2f%s)" h (s *. 100.) "%" (l *. 100.)
      "%"
  else
    Printf.sprintf "hsla(%.2f, %.2f%s, %.2f%s, %.2f)" h (s *. 100.) "%"
      (l *. 100.) "%" a

let to_css_rgba color =
  let color' = to_rgba color in
  if color'.a = 1. then
    Printf.sprintf "rgb(%d, %d, %d)" color'.r color'.g color'.b
  else
    Printf.sprintf "rgba(%d, %d, %d, %.2f)" color'.r color'.g color'.b color'.a

let black = Gg.Color.black

let white = Gg.Color.white

let gray_tone l = of_hsl 0. 0. l

let rotate_hue angle t =
  let {Hsla.h; s; l; a} = to_hsla t in
  of_hsla (h +. angle) s l a

let complementary = rotate_hue 180.

let lighten f t =
  let {Hsla.h; s; l; a} = to_hsla t in
  of_hsla h s (l +. f) a

let darken f = lighten (0. -. f)

let saturate f t =
  let {Hsla.h; s; l; a} = to_hsla t in
  of_hsla h (s +. f) l a

let desaturate f = saturate (0. -. f)

let brightness t =
  let {Rgba'.r; g; b; _} = to_rgba' t in
  ((299. *. r) +. (587. *. g) +. (114. *. b)) /. 1000.

let relative_luminance t =
  let {Rgba'.r; g; b; _} = to_rgba' t in
  let convert c =
    if c <= 0.03928 then c /. 12.92 else ((c +. 0.055) /. 1.055) ** 2.4
  in
  let r' = convert r in
  let g' = convert g in
  let b' = convert b in
  (0.2126 *. r') +. (0.7152 *. g') +. (0.0722 *. b')

let contrast_ratio t1 t2 =
  let l1 = relative_luminance t1 in
  let l2 = relative_luminance t2 in
  if l1 > l2 then (l1 +. 0.05) /. (l2 +. 0.05) else (l2 +. 0.05) /. (l1 +. 0.05)

let light t = brightness t > 0.5

let readable t1 t2 = contrast_ratio t1 t2 > 4.5

let text_color t = if light t then black else white
